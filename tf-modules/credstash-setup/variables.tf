variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}

variable "create_kms_key" {
  default     = true
  description = "Should the Master key be created"
}

variable "kms_key_name" {
  default     = "credstash"
  description = "KMS Master Key Name."
}

variable "enable_key_rotation" {
  default     = false
  description = "Specifies whether key rotation is enabled"
}

variable "create_db_table" {
  default     = true
  description = "Should the DynamoDB table be created"
}

variable "db_table_name" {
  default     = "credential-store"
  description = "Name of the DynamoDB table where credentials will be stored"
}

variable "create_reader_policy" {
  default     = false
  description = "Should credstash Secret Reader IAM Policy be created."
}

variable "create_writer_policy" {
  default     = false
  description = "Should credstash Secret Writer IAM Policy be created."
}
