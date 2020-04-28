variable "bucket_name" {
  type        = string
  description = "Bucket name of the encrypted S3 bucket"
}

variable "key_arn" {
  type = string
  description = "The KMS key set for encrypting the bucket"
}

variable "reader_role_name" {
  type        = string
  description = "The IAM role name to attach reading policy"
}
