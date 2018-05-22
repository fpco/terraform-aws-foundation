variable "name" {
  description = "Name to give bucket"
  type        = "string"
}

variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
  type        = "string"
}
