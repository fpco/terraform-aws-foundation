variable "create_kms_key" {
  default = true
  description = "Should the Master key be created"
}

variable "kms_key_name" {
  default = "credstash"
  description = "KMS Master Key Name."
}

variable "kms_key_policy" {
  default = ""
  description = "A valid policy JSON document"
}

variable "enable_key_rotation" {
  default = true
  description = "Specifies whether key rotation is enabled"
}

variable "create_db_table" {
  default = true
  description = "Should the DynamoDB table be created"
}

variable "db_table_name" {
  default = "credential-store"
  description = "Name of the DynamoDB table where credentials will be stored"
}
