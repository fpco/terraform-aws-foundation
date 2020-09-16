variable "key_count" {
  description = "Set to 1 to optionally create access keys for the created IAM user"
  default     = "0"
}

variable "vault_iam_username" {
  description = "Name of the IAM user created for Vault"
  type        = string
  default     = "test-env-vault-admin"
}

variable "vault_iam_container_name" {
  default     = "test-env-vault"
  description = "Name prefix for IAM users this policy provides ADMIN over"
  type        = string
}

