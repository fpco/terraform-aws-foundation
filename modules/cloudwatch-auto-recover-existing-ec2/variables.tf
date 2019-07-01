variable "name_prefix" {
  description = "Name to be prefixed to all resources"
  default     = ""
  type        = string
}

variable "name_format" {
  description = "Naming scheme as a string, to use with the format() function."
  default     = "%s-auto-recover-%02d"
  type        = string
}

variable "ec2_instance_ids" {
  description = "EC2 instance ids of VMs that should receive auto-recover capabilities."
  default     = []
  type        = list(string)
}

variable "maximum_failure_duration" {
  description = "Maximum amount of system status checks failures period in seconds before recovery kicks in."
  default     = "60"
  type        = string
}

variable "alarm_actions" {
  description = "list of alarm actions to append to the default (optional)"
  default     = []
  type        = list(string)
}

