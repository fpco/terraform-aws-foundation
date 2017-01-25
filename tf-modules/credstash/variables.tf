variable "name_prefix" {
  description = "The name prefix Reader/Writer Policies"
}
variable "create_reader_policy" {
  default = "false"
  description = "Should credstash Secret Reader IAM Policy be created."
}
variable "create_writer_policy" {
  default = "false"
  description = "Should credstash Secret Writer IAM Policy be created."
}
variable "region" {
  default = ""
  description = "Region where KMS key and DynamoDB with 'credential-store' table is located. Current region will be used if empty"
}
variable "account_id" {
  default = ""
  description = "Accound ID, to which KMS key and DynamoDB with 'credential-store' table belongs to. Current Account ID will be used if empty"
}
variable "kms_key_id" {
  default = ""
  description = "KMS key GUID"
}
