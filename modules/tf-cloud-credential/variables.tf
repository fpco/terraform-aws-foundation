variable "workspace_id" {
  type        = string
  description = "Id of workspace to put variables in."
}

variable "iam_access_key" {
  type = object({
    id     = string
    secret = string
  })
  description = "The aws_iam_access_key id/secret pair to use as credentials for the workspace."
}

variable "region" {
  type        = string
  description = "The aws region"
}
