# if we have no private_ips in our list, use the subnet_ids to determine how many
# EC2 instances to create. This lets the module create N instances across M subnets
# (interleave), or N instances across N subnets.
data "template_file" "cnt" {
  template = "$${ip_count > 0 ? ip_count : subnet_count}"

  vars = {
    ip_count     = length(var.private_ips)
    subnet_count = length(var.subnet_ids)
  }
}

# The instance running the DNS server
resource "aws_instance" "auto-recover" {
  count                       = data.template_file.cnt.rendered
  ami                         = var.ami
  instance_type               = var.instance_type
  iam_instance_profile        = data.template_file.cnt.rendered > 0 ? element(concat(var.iam_profiles, [""]), count.index) : ""
  subnet_id                   = element(var.subnet_ids, count.index)
  vpc_security_group_ids      = var.security_group_ids
  associate_public_ip_address = var.public_ip
  source_dest_check           = var.source_dest_check
  private_ip                  = data.template_file.cnt.rendered > 0 ? element(concat(var.private_ips, [""]), count.index) : ""
  key_name                    = var.key_name

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

  tags = merge(
    {
      "Name" = format(var.name_format, var.name_prefix, count.index + 1)
    },
    var.extra_tags,
  )

  lifecycle {
    ignore_changes = [ami]
  }

  user_data = element(var.user_data, count.index)
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
  count               = data.template_file.cnt.rendered
  alarm_name          = format(var.name_format, var.name_prefix, count.index + 1)
  metric_name         = var.metric_name
  comparison_operator = var.comparison_operator
  evaluation_periods  = var.evaluation_periods

  dimensions = {
    InstanceId = aws_instance.auto-recover[count.index].id
  }

  namespace         = var.namespace
  period            = var.max_failure_duration
  statistic         = var.statistic
  threshold         = var.threshold
  alarm_description = var.alarm_description
  alarm_actions = compact(
    concat(
      [
        "arn:${data.aws_partition.current.partition}:automate:${data.aws_region.current.name}:ec2:recover",
      ],
      var.alarm_actions,
    ),
  )
}

