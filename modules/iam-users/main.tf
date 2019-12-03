variable "user_list" {
  description = "List of strings, where each entry is the intended username for a user in IAM"
  type        = list(string)
}

variable "path" {
  default     = "/"
  description = "string path in IAM, for the users created (all get the same path)"
  type        = string
}

resource "aws_iam_user" "u" {
  count = length(var.user_list)
  name  = var.user_list[count.index]
}

output "users" {
  description = "The list of IAM user objects that exist"
  value       = zipmap(var.user_list, slice(aws_iam_user.u.*, 0, length(var.user_list)))
}
