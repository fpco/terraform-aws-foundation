/**
 * ## Simple DNS server(s) running `bind9`
 *
 * Supports setting up multiple servers in different AZs using the same
 * configuration.
 *
 * Currently the intent is for setting up caching/forwarding
 * servers on private VPCs (as Route53 provides serving DNS zones well).
 *
 * The `null_resource` provisioner will upload new options
 * and reload the configuration whenever it changes.
 *
 * TODO: support uploading additional files, such as DNS zones
 *
 * ### Example
 *
 *     module "dns" {
 *       source  = "../../vendor/fpco/terraform-aws/tf-modules/bind-server"
 *       name = "dns"
 *       ami = "ami-7c803d1c" #Ubuntu 16.04
 *       subnet_ids = "${module.vpc.private_subnets}"
 *       private_ips  = ["${var.private_dns_ips}"]
 *       security_group_ids = ["${aws_security_group.dns.id}"]
 *       key_name = "${aws_key_pair.admin.id}"
 *       named_conf_options = "${file("${path.module}/files/dns/named.conf.options")}"
 *     }
 *
 * And `files/dns/named.conf.options` would be something like this:
 *
 *     options {
 *     	directory "/var/cache/bind";
 *     	forward only;
 *       # This is the Amazon DNS server, which is always at the VPC's '+2' address
 *     	forwarders {
 *     	 	10.0.0.2;
 *     	};
 *     	dnssec-validation auto;
 *
 *     	auth-nxdomain no;    # conform to RFC1035
 *     	listen-on-v6 { any; };
 *
 *     	allow-query { any; };
 *     };
 *     # This is a private zone served by Route 53 that uses the Amazon DNS server
 *     zone "route53.example.com." {
 *     	type forward;
 *     	forward only;
 *     	forwarders { 10.0.0.2; };
 *     };
 *     # This is the corporate domain that forwards/caches the corporate DNS servers
 *     zone "example.com." {
 *     	type forward;
 *     	forwarders { 10.10.1.50; 10.10.1.51; };
 *     };
 *
 * Note that the instance has auto-recovery enabled, so in case there is a
 * hardware failure, the instance will be recreated on a new hardware node (but
 * retaining the same instance ID and ip addresses).
 *
 */

# The instance running the DNS server
resource "aws_instance" "bind" {
  count                  = "${length(var.private_ips)}"
  ami                    = "${var.ami}"
  instance_type          = "${var.instance_type}"
  subnet_id              = "${element(var.subnet_ids, count.index)}"
  vpc_security_group_ids = ["${var.security_group_ids}"]
  private_ip             = "${var.private_ips[count.index]}"
  key_name               = "${var.key_name}"

  root_block_device {
    volume_type = "gp2"
    volume_size = 8
  }

  # Instance auto-recovery (see cloudwatch metric alarm below) doesn't support
  # instances with ephemeral storage, so this disables it.
  # See https://github.com/hashicorp/terraform/issues/5388#issuecomment-282480864
  ephemeral_block_device {
    device_name = "/dev/sdb"
    no_device   = true
  }

  ephemeral_block_device {
    device_name = "/dev/sdc"
    no_device   = true
  }

  tags {
    Name = "${length(var.names) == 0 ? format("%s-%02d", var.name, count.index + 1) : var.names[count.index]}"
  }

  lifecycle {
    ignore_changes = ["key_name"]
  }

  provisioner "remote-exec" {
    connection {
      host                = "${self.private_ip}"
      user                = "${var.distro == "ubuntu" ? "ubuntu" : "ec2-user"}"
      private_key         = "${file(var.ssh_key)}"
      bastion_host        = "${var.bastion_host}"
      bastion_user        = "${var.bastion_user}"
      bastion_private_key = "${var.bastion_private_key}"
    }

    inline = [
      "${var.distro == "ubuntu" ? "sudo apt-get update && sudo apt-get install -y bind9 dnsutils && sudo service bind9 start" : "sudo yum install -y bind && sudo service named start && sudo chkconfig named on"}",
    ]
  }
}

data "template_file" "config_root" {
  template = "${var.distro == "ubuntu" ? "/etc/bind" : "/etc"}"
}

data "template_file" "config_owner" {
  template = "${var.distro == "ubuntu" ? "root:bind" : "root:named"}"
}

# Contains provisioner that is triggered whenever named options are changed.
resource "null_resource" "bind" {
  count = "${length(var.private_ips)}"

  triggers {
    named_conf         = "${var.named_conf}"
    named_conf_options = "${var.named_conf_options}"
    named_conf_local   = "${var.named_conf_local}"
    log_files          = "${join("|", var.log_files)}"
    instance_id        = "${aws_instance.bind.*.id[count.index]}"
  }

  connection {
    host                = "${aws_instance.bind.*.private_ip[count.index]}"
    user                = "${var.distro == "ubuntu" ? "ubuntu" : "ec2-user"}"
    private_key         = "${file(var.ssh_key)}"
    bastion_host        = "${var.bastion_host}"
    bastion_user        = "${var.bastion_user}"
    bastion_private_key = "${var.bastion_private_key}"
  }

  provisioner "file" {
    content     = "${var.named_conf}"
    destination = "/tmp/named.conf"
  }

  provisioner "file" {
    content     = "${var.named_conf_options}"
    destination = "/tmp/named.conf.options"
  }

  provisioner "file" {
    content     = "${var.named_conf_local}"
    destination = "/tmp/named.conf.local"
  }

  provisioner "remote-exec" {
    inline = [
      "mkdir --parents /tmp/db_records",
    ]
  }

  provisioner "file" {
    source      = "${var.db_records_folder}/"
    destination = "/tmp/db_records"
  }

  provisioner "remote-exec" {
    inline = [
      "sudo chown ${data.template_file.config_owner.rendered} /tmp/named.conf",
      "${var.named_conf == "//" ? "sudo rm /tmp/named.conf" : join("", list("sudo mv /tmp/named.conf ", data.template_file.config_root.rendered, "/named.conf"))}",
      "sudo chown ${data.template_file.config_owner.rendered} /tmp/named.conf.options",
      "${var.named_conf_options == "//" ? "sudo rm /tmp/named.conf.options" : join("", list("sudo mv /tmp/named.conf.options ", data.template_file.config_root.rendered, "/named.conf.options"))}",
      "sudo chown ${data.template_file.config_owner.rendered} /tmp/named.conf.local",
      "${var.named_conf_local == "//" ? "sudo rm /tmp/named.conf.local" : join("", list("sudo mv /tmp/named.conf.local ", data.template_file.config_root.rendered, "/named.conf.local"))}",
      "sudo chown -R ${data.template_file.config_owner.rendered} /tmp/db_records/*",
      "${var.db_records_folder == "" ? "sudo rm /tmp/db_records/*; sudo rmdir /tmp/db_records" : join("", list("sudo mv /tmp/db_records/* ", data.template_file.config_root.rendered))}",
      "${formatlist("sudo mkdir -p \"$(dirname '%s')\"", var.log_files)}",
      "${formatlist("sudo touch \"$(dirname '%s')\"", var.log_files)}",
      "${formatlist("sudo chown bind \"$(dirname '%s')\"", var.log_files)}",
      "sudo killall -HUP named",
    ]
  }
}

# Current AWS region
data "aws_region" "current" {
  current = true
}

# Cloudwatch alarm that recovers the instance after two minutes of system status check failure
resource "aws_cloudwatch_metric_alarm" "auto-recover" {
  count               = "${length(compact(var.private_ips))}"
  alarm_name          = "${length(var.names) == 0 ? format("%s-%02d", var.name, count.index) : var.names[count.index]}"
  metric_name         = "StatusCheckFailed_System"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = "2"

  dimensions {
    InstanceId = "${aws_instance.bind.*.id[count.index]}"
  }

  namespace         = "AWS/EC2"
  period            = "60"
  statistic         = "Minimum"
  threshold         = "0"
  alarm_description = "Auto-recover the instance if the system status check fails for two minutes"
  alarm_actions     = ["${compact(concat(list("arn:${var.aws_cloud}:automate:${data.aws_region.current.name}:ec2:recover"), "${var.alarm_actions}"))}"]
}
