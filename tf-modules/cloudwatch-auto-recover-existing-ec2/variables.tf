variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}

variable "name_prefix" {
  description = "Name to be prefixed to all resources"
  default     = ""
}

variable "name_format" {
  description = "Naming scheme as a string, to use with the format() function."
  default     = "%s-auto-recover-%02d"
}

variable "ec2_instance_ids" {
  description = "EC2 instance ids of VMs that should receive auto-recover capabilities."
  type        = "list"
  default     = []
}

variable "maximum_failure_duration" {
  description = "Maximum amount of system status checks failures period in seconds before recovery kicks in."
  default     = "60"
}

variable "alarm_actions" {
  description = "list of alarm actions to append to the default (optional)"
  default     = []
}
