variable "name" {
  description = "Prefix of the AutoScaling Policy."
  type        = string
}

variable "asg_name" {
  description = "Name of the AutoScaling Group that this policy would apply to."
  type        = string
}

variable "metric" {
  description = "CPUUtilization or MemoryUtilization"
  type        = string
}

variable "up_evaluation_periods" {
  default     = 3
  description = ""
  type        = number
}

variable "up_period" {
  default     = 60
  description = ""
  type        = number
}

variable "up_threshold" {
  default     = 60
  description = ""
  type        = number
}

variable "up_scaling_adjustment" {
  default     = 3
  description = ""
  type        = number
}

variable "up_cooldown" {
  default     = 300
  description = ""
  type        = number
}

variable "down_evaluation_periods" {
  default     = 3
  description = ""
  type        = number
}

variable "down_period" {
  default     = 60
  description = ""
  type        = number
}

variable "down_threshold" {
  default     = 30
  description = ""
  type        = number
}

variable "down_scaling_adjustment" {
  default     = 2
  description = ""
  type        = number
}

variable "down_cooldown" {
  default     = 300
  description = ""
  type        = number
}

resource "aws_autoscaling_policy" "scale_up" {
  name                   = "${var.name}-asp-up"
  autoscaling_group_name = var.asg_name
  adjustment_type        = "ChangeInCapacity"
  cooldown               = var.up_cooldown
  scaling_adjustment     = var.up_scaling_adjustment
  policy_type            = "SimpleScaling"
}

resource "aws_cloudwatch_metric_alarm" "scale_up" {
  alarm_name          = "${var.name}-scale-up-alarm"
  comparison_operator = "GreaterThanOrEqualToThreshold"
  evaluation_periods  = var.up_evaluation_periods
  metric_name         = var.metric
  namespace           = "AWS/EC2"
  period              = var.up_period
  statistic           = "Average"
  threshold           = var.up_threshold
  dimensions = {
    AutoScalingGroupName = var.asg_name
  }
  alarm_actions = [aws_autoscaling_policy.scale_up.arn]
}

resource "aws_autoscaling_policy" "scale_down" {
  name                   = "${var.name}-asp-down"
  autoscaling_group_name = var.asg_name
  adjustment_type        = "ChangeInCapacity"
  cooldown               = var.down_cooldown
  scaling_adjustment     = "-${var.down_scaling_adjustment}"
  policy_type            = "SimpleScaling"
}

resource "aws_cloudwatch_metric_alarm" "scale_down" {
  alarm_name          = "${var.name}-scale-down-alarm"
  comparison_operator = "LessThanOrEqualToThreshold"
  evaluation_periods  = var.down_evaluation_periods
  metric_name         = var.metric
  namespace           = "AWS/EC2"
  period              = var.down_period
  statistic           = "Average"
  threshold           = var.down_threshold
  dimensions = {
    AutoScalingGroupName = var.asg_name
  }
  alarm_actions = [aws_autoscaling_policy.scale_down.arn]
}

