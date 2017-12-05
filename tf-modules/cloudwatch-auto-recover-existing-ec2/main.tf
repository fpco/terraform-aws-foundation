/**
 * ## Cloud Watch EC2 Auto-Recovery
 *
 * Document.
 *
 */

data "aws_region" "current" {
  current = true
}

resource "aws_cloudwatch_metric_alarm" "auto-recover" {
  count               = "${length(compact(var.ec2_instance_ids))}"
  alarm_name          = "${format(var.name_format, var.name_prefix, count.index + 1)}"
  metric_name         = "StatusCheckFailed_System"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = "2"

  dimensions {
    InstanceId = "${var.ec2_instance_ids[count.index]}"
  }

  namespace         = "AWS/EC2"
  period            = "60"
  statistic         = "Minimum"
  threshold         = "0"
  alarm_description = "Auto-recover the instance if the system status check fails for two minutes"
  alarm_actions     = ["${compact(concat(list("arn:${var.aws_cloud}:automate:${data.aws_region.current.name}:ec2:recover"), "${var.alarm_actions}"))}"]
}
