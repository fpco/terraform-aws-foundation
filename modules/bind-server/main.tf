# The instance running the DNS server
resource "aws_instance" "bind" {
  count                  = length(compact(var.private_ips))
  ami                    = var.ami
  instance_type          = var.instance_type
  subnet_id              = element(var.subnet_ids, count.index)
  vpc_security_group_ids = var.security_group_ids
  private_ip             = var.private_ips[count.index]
  key_name               = var.key_name
  iam_instance_profile   = ""

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

  tags = {
    Name = length(var.names) == 0 ? format("%s-%02d", var.name, count.index + 1) : var.names[count.index]
  }

  lifecycle {
    ignore_changes = [key_name]
  }

  provisioner "remote-exec" {
    connection {
      type                = "ssh"
      host                = self.private_ip
      user                = var.distro == "ubuntu" ? "ubuntu" : "ec2-user"
      private_key         = file(var.ssh_key)
      bastion_host        = var.bastion_host
      bastion_user        = var.bastion_user
      bastion_private_key = var.bastion_private_key
    }

    inline = [
      var.distro == "ubuntu" ? "sudo apt-get update && sudo apt-get install -y bind9 dnsutils && sudo service bind9 start" : "sudo yum install -y bind && sudo service named start && sudo chkconfig named on",
    ]
  }
}

data "template_file" "config_root" {
  template = var.distro == "ubuntu" ? "/etc/bind" : "/etc"
}

data "template_file" "config_owner" {
  template = var.distro == "ubuntu" ? "root:bind" : "root:named"
}

# Contains provisioner that is triggered whenever named options are changed.
resource "null_resource" "bind" {
  count = length(compact(var.private_ips))

  triggers = {
    named_conf         = var.named_conf
    named_conf_options = var.named_conf_options
    named_conf_local   = var.named_conf_local
    log_files          = join("|", var.log_files)
    instance_id        = aws_instance.bind[count.index].id
  }

  connection {
    type                = "ssh"
    host                = aws_instance.bind[count.index].private_ip
    user                = var.distro == "ubuntu" ? "ubuntu" : "ec2-user"
    private_key         = file(var.ssh_key)
    bastion_host        = var.bastion_host
    bastion_user        = var.bastion_user
    bastion_private_key = var.bastion_private_key
  }

  provisioner "file" {
    content     = var.named_conf
    destination = "/tmp/named.conf"
  }

  provisioner "file" {
    content     = var.named_conf_options
    destination = "/tmp/named.conf.options"
  }

  provisioner "file" {
    content     = var.named_conf_local
    destination = "/tmp/named.conf.local"
  }

  provisioner "remote-exec" {
    inline = [
      "mkdir --parents /tmp/db_records",
    ]
  }

  provisioner "file" {
    source      = var.db_records_folder == "" ? "${path.module}/templates/db_records/" : "${var.db_records_folder}/"
    destination = "/tmp/db_records"
  }

  provisioner "remote-exec" {
    inline = concat([
      "sudo chown ${data.template_file.config_owner.rendered} /tmp/named.conf",
      var.named_conf == "//" ? "sudo rm /tmp/named.conf" : join(
        "",
        [
          "sudo mv /tmp/named.conf ",
          data.template_file.config_root.rendered,
          "/named.conf",
        ],
      ),
      "sudo chown ${data.template_file.config_owner.rendered} /tmp/named.conf.options",
      var.named_conf_options == "//" ? "sudo rm /tmp/named.conf.options" : join(
        "",
        [
          "sudo mv /tmp/named.conf.options ",
          data.template_file.config_root.rendered,
          "/named.conf.options",
        ],
      ),
      "sudo chown ${data.template_file.config_owner.rendered} /tmp/named.conf.local",
      var.named_conf_local == "//" ? "sudo rm /tmp/named.conf.local" : join(
        "",
        [
          "sudo mv /tmp/named.conf.local ",
          data.template_file.config_root.rendered,
          "/named.conf.local",
        ],
      ),
      "sudo chown -R ${data.template_file.config_owner.rendered} /tmp/db_records/*",
      var.db_records_folder == "" ? "sudo rm /tmp/db_records/*; sudo rmdir /tmp/db_records" : join(
        "",
        [
          "sudo mv /tmp/db_records/* ",
          data.template_file.config_root.rendered,
        ],
      )],
      formatlist("sudo mkdir -p \"$(dirname '%s')\"", var.log_files),
      formatlist("sudo touch \"$(dirname '%s')\"", var.log_files),
      formatlist("sudo chown bind \"$(dirname '%s')\"", var.log_files),
      ["sudo killall -HUP named"])
  }
}

# Current AWS region
data "aws_region" "current" {
}

# Lookup the current AWS partition
data "aws_partition" "current" {
}

# Cloudwatch alarm that recovers the instance after two minutes of system status check failure
resource "aws_cloudwatch_metric_alarm" "auto-recover" {
  count               = length(compact(var.private_ips))
  alarm_name          = length(var.names) == 0 ? format("%s-%02d", var.name, count.index) : var.names[count.index]
  metric_name         = "StatusCheckFailed_System"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = "2"

  dimensions = {
    InstanceId = aws_instance.bind[count.index].id
  }

  namespace         = "AWS/EC2"
  period            = "60"
  statistic         = "Minimum"
  threshold         = "0"
  alarm_description = "Auto-recover the instance if the system status check fails for two minutes"
  alarm_actions = compact(
    concat(
      [
        "arn:${data.aws_partition.current.partition}:automate:${data.aws_region.current.name}:ec2:recover",
      ],
      var.alarm_actions,
    ),
  )
}

