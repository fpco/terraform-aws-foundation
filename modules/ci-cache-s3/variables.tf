variable "prefix" {
  description = "Name prefix for all of the resources"
  type        = string
}

variable "user_name" {
  description = "Username for the IAM user that will be created with full access to the newly created S3 bucket (will be prefixed by `prefix` variable)"
  default     = "ci-cache-user"
  type        = string
}

variable "bucket_name" {
  description = "Name for the S3 bucket (will be prefixed by `prefix` variable)"
  default     = "ci-cache"
  type        = string
}

variable "pgp_key" {
  description = "Name of PGP key to use for secret_key encryption, eg. keybase:username"
  type        = string
}

variable "cache_days" {
  description = "Number of days after which cache expires. Setting it to 0 will disable expiration"
  default     = 7
  type        = string
}

variable "user_grants_public" {
  description = "There is a separate capability of cache-s3 to make cache publically readable with --public flag, which has to be explicitly allowed at AWS level"
  default     = false
  type        = string
}

