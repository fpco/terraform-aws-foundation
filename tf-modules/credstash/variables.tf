variable "name_prefix" {
  description = "The name prefix Reader/Writer Policies"
}

variable "kms_key_arn" {
  description = "Master KMS key ARN"
}

variable "create_reader_policy" {
  default = false
  description = "Should credstash Secret Reader IAM Policy be created."
}

variable "create_writer_policy" {
  default = false
  description = "Should credstash Secret Writer IAM Policy be created."
}

variable "region" {
  default = ""
  description = "Region where DynamoDB with 'var.table_name' table is located. Current region will be used if empty"
}

variable "account_id" {
  default = ""
  description = "Accound ID, to which DynamoDB with 'var.table_name' table belongs to. Current Account ID will be used if empty"
}

variable "table_name" {
  default = "credential-store"
  description = "Name of the DynamoDB table where credentials will be stored"
}
