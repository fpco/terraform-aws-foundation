variable "name_prefix" {
  description = "name to be prepended to all resources"
}

variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}

variable "kms_key_id" {
  description = "KMS key ARN for encryption of logs"
  default     = ""
}

variable "include_global_service_events" {
  description = "boolean, if true logs IAM events as well as region-specific ones"
  default     = true
}

variable "enable_logging" {
  description = "boolean, defines if logging should be started or stopped"
  default     = true
}

variable "extra_tags" {
  default = {}
}
