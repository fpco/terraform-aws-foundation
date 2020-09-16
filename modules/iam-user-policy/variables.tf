variable "username" {
  description = "Name of the IAM user that needs to be created"
  type        = string
}

variable "extra_tags" {
  description = "Extra tags to associate with the IAM user"
  default     = {}
  type        = map
}

variable "iam_policy_name" {
  description = "Policy name for the IAM user"
  type        = string
}

variable "iam_user_policy" {
  description = "The policy document. This is a JSON formatted string."
  type        = string
}
