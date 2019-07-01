# The instance running the DNS server
resource "aws_instance" "dnsmasq" {
  count                  = length(var.private_ips)
  ami                    = var.ami
  instance_type          = var.instance_type
  subnet_id              = element(var.subnet_ids, count.index)
  vpc_security_group_ids = var.security_group_ids
  private_ip             = var.private_ips[count.index]
  key_name               = var.key_name
  iam_instance_profile   = ""

  root_block_device {
    volume_type = var.root_volume_type
    volume_size = var.root_volume_size
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
    Name = format(var.name_format, var.name_prefix, count.index + 1)
  }

  lifecycle {
    ignore_changes = [ami]
  }

  user_data = var.user_data
}

# Contains provisioner that is triggered whenever dnsmasq options are changed.
resource "null_resource" "dnsmasq" {
  count = length(var.private_ips)

  triggers = {
    dnsmasq_conf = var.dnsmasq_conf
    instance_id  = aws_instance.dnsmasq[count.index].id
  }

  connection {
    type                = "ssh"
    host                = aws_instance.dnsmasq[count.index].private_ip
    user                = var.ssh_user
    private_key         = file(var.ssh_key)
    bastion_host        = var.bastion_host
    bastion_user        = var.bastion_user
    bastion_private_key = var.bastion_private_key
  }

  provisioner "file" {
    content     = var.dnsmasq_conf
    destination = "/tmp/dnsmasq.conf"
  }

  # this could be more cross-platform, but got to move on to other things
  # this could be more cross-platform, but got to move on to other things
  provisioner "remote-exec" {
    inline = [
      var.pre_init,
      "sudo mv /tmp/dnsmasq.conf /etc/",
      "sudo chown root:root /etc/dnsmasq.conf",
      "sudo chmod 640 /etc/dnsmasq.conf",
      "sudo service dnsmasq restart",
      var.post_init,
    ]
  }
}

# Current AWS region
data "aws_region" "current" {
}

# Lookup the current AWS partition
data "aws_partition" "current" {
}

# Cloudwatch alarm that recovers the instance after two minutes of system status
# check failure
resource "aws_cloudwatch_metric_alarm" "auto-recover" {
  count               = length(compact(var.private_ips))
  alarm_name          = format(var.name_format, var.name_prefix, count.index + 1)
  metric_name         = "StatusCheckFailed_System"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = "2"

  dimensions = {
    InstanceId = aws_instance.dnsmasq[count.index].id
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

