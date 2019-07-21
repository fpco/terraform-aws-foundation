variable "application" {
  description = "Application name"
  default     = "s3-vault-demo"
  type        = string
}

variable "environment" {
  description = "Environment name: dev/qa/prod etc"
  default     = "dev"
  type        = string
}

variable "vault_address" {
  description = "URL for the Vault server"
  type        = string
}

variable "vault_token" {
  description = "Vault token needed for authorization to the server"
  default     = "xxx"
}

variable "region" {
  description = "AWS Region"
  type        = string
  default     = "us-east-2"
}

variable "secret_backend_path" {
  description = "Unique AWS secret path for mouting"
  type        = string
  default     = "fpco/aws/dev/vault"
}

variable "default_lease_ttl_seconds" {
  description = "The default TTL for credentials issued by this backend."
  type        = string
  default     = "900"
}

variable "max_lease_ttl_seconds" {
  description = "The maximum TTL that can be requested for credentials issued by this backend."
  type        = string
  default     = "900"
}

variable "credential_type" {
  description = "Specifies the type of credential to be used when retrieving credentials from the role."
  type        = string
  default     = "assumed_role"
}

variable "access_key" {
  description = "The AWS Access Key ID this backend should use to issue new credentials."
  type        = string
}

variable "secret_key" {
  description = "The AWS Secret Key this backend should use to issue new credentials."
  type        = string
}

variable "role_name" {
  description = "The name to identify this role within the backend. Must be unique within the backend."
  type        = string
  default     = "s3_app_user"
}

