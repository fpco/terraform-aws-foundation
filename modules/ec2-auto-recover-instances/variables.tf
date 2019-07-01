variable "ami" {
  description = "AMI to use for instances.  Tested with Ubuntu 16.04."
  type        = string
}

variable "extra_tags" {
  description = "map of tags to append to the Name tag, added to the instance"
  default     = {}
  type        = map(string)
}

variable "instance_type" {
  description = "Type of instance to run the servers"
  default     = "t2.nano"
  type        = string
}

variable "iam_profiles" {
  description = "maps to `aws_instance.iam_instance_profile`, for each instance"
  default     = []
  type        = list(string)
}

variable "subnet_ids" {
  description = "Subnets to run servers in"
  type        = list(string)
}

variable "public_ip" {
  description = "boolean to enable / disable public IP addresses for the instances"
  type        = string
  default     = false
}

variable "private_ips" {
  description = "Private IP addresses of servers, which must be within the subnets specified in 'subnet_ids' (in the same order).  These are specified explicitly since it's desirable to be able to replace a DNS server without its IP address changing.  Our convention is to use the first unreserved address in the subnet (which is to say, the '+4' address)."
  type        = list(string)
}

variable "source_dest_check" {
  description = "boolean to enable / disable source_dest_check on the instances"
  type        = string
  default     = true
}

variable "security_group_ids" {
  description = "Security groups to assign servers"
  type        = list(string)
}

variable "key_name" {
  description = "SSH key-pair name to use for setup"
  type        = string
}

variable "name_prefix" {
  description = "Name prefix of the instances (will append 'dns-master-XX')"
  type        = string
}

variable "name_format" {
  default     = "%s-auto-recover-%02d"
  description = "naming scheme as a string, to use with the format() function"
  type        = string
}

variable "user_data" {
  description = "list of shell script code to pass to each `aws_instance.user_data`"
  default     = []
  type        = list(string)
}

variable "alarm_actions" {
  description = "list of alarm actions to append to the default (optional)"
  default     = []
  type        = list(string)
}

variable "root_volume_type" {
  default     = "gp2"
  description = "Type of EBS volume to use for the root block device"
  type        = string
}

variable "root_volume_size" {
  default     = "8"
  description = "Size (in GB) of EBS volume to use for the root block device"
  type        = string
}

variable "max_failure_duration" {
  description = "seconds, maps to `period` in `aws_cloudwatch_metric_alarm`"
  default     = "60"
  type        = string
}

variable "metric_name" {
  description = "maps to `metric_name` in `aws_cloudwatch_metric_alarm`"
  default     = "StatusCheckFailed_System"
  type        = string
}

variable "comparison_operator" {
  description = "maps to `comparison_operator` in `aws_cloudwatch_metric_alarm`"
  default     = "GreaterThanThreshold"
  type        = string
}

variable "evaluation_periods" {
  description = "maps to `evaluation_periods` in `aws_cloudwatch_metric_alarm`"
  default     = "2"
  type        = string
}

variable "statistic" {
  description = "maps to `statistic` in `aws_cloudwatch_metric_alarm`"
  default     = "Minimum"
  type        = string
}

variable "threshold" {
  description = "maps to `threshold` in `aws_cloudwatch_metric_alarm`"
  default     = "0"
  type        = string
}

variable "alarm_description" {
  description = "maps to `alarm_description` in `aws_cloudwatch_metric_alarm`"
  default     = "Auto-recover the instance if the system status check fails for two minutes"
  type        = string
}

variable "namespace" {
  description = "maps to `namespace` in `aws_cloudwatch_metric_alarm`"
  default     = "AWS/EC2"
  type        = string
}

