/**
 * ## Simple DNS server(s) running dnsmasq.
 *
 * Supports setting up multiple servers in different AZs using the same
 * configuration.
 *
 * The intent is for setting up dnsmasq for private zones associated with a VPC
 *
 * The `null_resource` provisioner will upload new options and reload the
 * configuration whenever it changes.
 *
 * ### Example
 *
 *     data "template_file" "dnsmasq_conf" {
 *       vars {
 *         vpc_amazon_dns_ip     = "${var.vpc_amazon_dns_ip}"
 *         private_dns_zone_name = "${var.private_dns_zone_name}"
 *         ns1                   = "${var.ns[0]}"
 *         ns2                   = "${var.ns[1]}"
 *       }
 *       template = <<END_CONF
 *     # DNSMASQ CONFIG
 *     #
 *     domain-needed
 *     bogus-priv
 *     expand-hosts
 *     domain=${private_dns_zone_name}
 *     local=/${private_dns_zone_name}/
 *     server=${vpc_amazon_dns_ip}
 *     server=${ns1}
 *     server=${ns2}
 *     END_CONF
 *     }
 *     
 *     # The DNS servers
 *     module "dns" {
 *       source = "../../vendor/fpco-terraform-aws/tf-modules/dnsmasq-server"
 *       name_prefix         = "${var.name}"
 *       dnsmasq_conf        = "${data.template_file.dnsmasq_conf.rendered}"
 *       ami                 = "${data.aws_ami.ubuntu-xenial.id}"
 *       key_name            = "${aws_key_pair.main.id}"
 *       ssh_key             = "${var.ssh_key}"
 *      #bastion_host        = "${aws_instance.bastion.public_dns}"
 *      #bastion_user        = "ubuntu"
 *      #bastion_private_key = "${var.ssh_key}"
 *       subnet_ids          = ["${module.private-subnets.ids}"]
 *       private_ips         = ["${var.private_dns_ips}"]
 *       security_group_ids  = [
 *         "${module.dns-server-sg.id}",
 *         "${module.public-ssh-sg.id}",
 *         "${module.open-egress-sg.id}",
 *       ]
 *       user_data = <<END_INIT
 *     ufw allow 53
 *     echo "10.10.0.10 foobar.${var.private_dns_zone_name}" >> /etc/hosts
 *     END_INIT
 *       alarm_actions = []
 *     }
 */

# The instance running the DNS server
resource "aws_instance" "dnsmasq" {
  count                  = "${length(var.private_ips)}"
  ami                    = "${var.ami}"
  instance_type          = "${var.instance_type}"
  subnet_id              = "${element(var.subnet_ids, count.index)}"
  vpc_security_group_ids = ["${var.security_group_ids}"]
  private_ip             = "${var.private_ips[count.index]}"
  key_name               = "${var.key_name}"

  root_block_device {
    volume_type = "${var.root_volume_type}"
    volume_size = "${var.root_volume_size}"
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
    Name = "${format(var.name_format, var.name_prefix, count.index + 1)}"
  }

  lifecycle {
    ignore_changes = ["ami"]
  }

  user_data = "${var.user_data}"
}

# Contains provisioner that is triggered whenever dnsmasq options are changed.
resource "null_resource" "dnsmasq" {
  count = "${length(var.private_ips)}"

  triggers {
    dnsmasq_conf = "${var.dnsmasq_conf}"
    instance_id  = "${aws_instance.dnsmasq.*.id[count.index]}"
  }

  connection {
    type                = "ssh"
    host                = "${aws_instance.dnsmasq.*.private_ip[count.index]}"
    user                = "${var.ssh_user}"
    private_key         = "${file(var.ssh_key)}"
    bastion_host        = "${var.bastion_host}"
    bastion_user        = "${var.bastion_user}"
    bastion_private_key = "${var.bastion_private_key}"
  }

  provisioner "file" {
    content     = "${var.dnsmasq_conf}"
    destination = "/tmp/dnsmasq.conf"
  }

  # this could be more cross-platform, but got to move on to other things
  provisioner "remote-exec" {
    inline = [
      "${var.pre_init}",
      "sudo mv /tmp/dnsmasq.conf /etc/",
      "sudo chown root:root /etc/dnsmasq.conf",
      "sudo chmod 640 /etc/dnsmasq.conf",
      "sudo service dnsmasq restart",
      "${var.post_init}",
    ]
  }
}

# Current AWS region
data "aws_region" "current" {
  current = true
}

# Cloudwatch alarm that recovers the instance after two minutes of system status
# check failure
resource "aws_cloudwatch_metric_alarm" "auto-recover" {
  count               = "${length(compact(var.private_ips))}"
  alarm_name          = "${format(var.name_format, var.name_prefix, count.index + 1)}"
  metric_name         = "StatusCheckFailed_System"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = "2"

  dimensions {
    InstanceId = "${aws_instance.dnsmasq.*.id[count.index]}"
  }

  namespace         = "AWS/EC2"
  period            = "60"
  statistic         = "Minimum"
  threshold         = "0"
  alarm_description = "Auto-recover the instance if the system status check fails for two minutes"
  alarm_actions     = ["${compact(concat(list("arn:${var.aws_cloud}:automate:${data.aws_region.current.name}:ec2:recover"), "${var.alarm_actions}"))}"]
}
