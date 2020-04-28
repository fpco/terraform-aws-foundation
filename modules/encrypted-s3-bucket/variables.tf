variable "bucket_name" {
  type        = string
  description = "Bucket name for the encrypted S3 bucket"
}

variable "reader_role_name" {
  type        = string
  default     = ""
  description = "The IAM role name to attach reading policy"
}

variable "writer_role_name" {
  type        = string
  default     = ""
  description = "The IAM role name to attach writing policy"
}
