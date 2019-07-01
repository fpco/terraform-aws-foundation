variable "name_prefix" {
  description = "name to be prepended to all resources"
  type        = string
}

variable "kms_key_id" {
  description = "KMS key ARN for encryption of logs"
  default     = ""
  type        = string
}

variable "include_global_service_events" {
  description = "boolean, if true logs IAM events as well as region-specific ones"
  default     = true
  type        = string
}

variable "enable_logging" {
  description = "boolean, maps to `aws_cloudtrail`'s enable_logging parameter"
  default     = true
  type        = string
}

variable "extra_tags" {
  description = "map of tags to append to AWS resources that support tagging"
  default     = {}
  type        = map(string)
}

