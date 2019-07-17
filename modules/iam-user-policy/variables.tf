variable "user_name" {
  description = "Name of the IAM user that needs to be created"
  type        = "string"
}

variable "stage" {
  description = "Environment stage to indicate whether it is prod/staging or dev. Used for tagging purpose."
  type        = "string"
  default     = "dev"
}

variable "iam_policy_name" {
  description = "Policy name for the IAM user"
  type        = "string"
}

variable "iam_user_policy" {
  description = "The policy document. This is a JSON formatted string."
  type        = "string"
}
