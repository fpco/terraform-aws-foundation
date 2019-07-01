variable "aws_region" {
  description = "AWS partition where this is running. Unless running on GovCloud, leave as default"
  default     = "aws"
  type        = string
}

variable "name_prefix" {
  description = "Name to prefix to S3 bucket with CloudWatch logs"
  type        = string
}

variable "principals" {
  description = "List of principals’ ARNs"
  type        = list(string)
}

variable "extra_tags" {
  description = "Tags to apply on S3 bucket. Name is automatically created, so no need to pass it."
  default     = {}
  type        = map(string)
}

