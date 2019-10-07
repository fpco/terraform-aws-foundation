variable "workspace_name_prefix" {
  type        = string
  description = "The name prefix to use for the workspace"
}

variable "organization" {
  type        = string
  description = "The workspace organization"
}

variable "iam_access_key" {
  type = object({
    id     = string
    secret = string
  })
  description = "The aws_iam_access_key to use as credentials for workspace."
}

variable "region" {
  type        = string
  description = "The aws region"
}
