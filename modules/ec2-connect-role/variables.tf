variable "name" {
  description = "Name to give the role"
  type        = string
}

variable "trust_account_ids" {
  description = "List of other accounts to trust to assume the role"
  default     = []
  type        = list(string)
}

variable "region" {
  description = "The AWS region to deploy to"
  type        = string
}

variable "account_id" {
  description = "ID of the account which instances to connect to"
  type        = string
}

variable "instance_ids" {
  description = "IDs of instances to connect to"
  type        = list(string)
  default     = ["*"]
}
