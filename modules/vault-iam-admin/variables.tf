variable "extra_tags" {
  description = "Extra tags to associate with the IAM user"
  default     = {}
  type        = map
}

variable "username" {
  description = "Name of the IAM user created for Vault"
  type        = string
}

variable "vault_iam_container_name" {
  description = "Name prefix for IAM users this policy provides ADMIN over"
  type        = string
}
