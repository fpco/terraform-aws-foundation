variable "user_name" {
  description = "Name of the IAM user that needs to be created"
  type        = string
}

variable "environment" {
  description = "Environment name: dev/qa/prod etc"
  default     = "dev"
  type        = string
}

variable "iam_policy_name" {
  description = "Policy name for the IAM user"
  type        = string
}

variable "iam_user_policy" {
  description = "The policy document. This is a JSON formatted string."
  type        = string
}

