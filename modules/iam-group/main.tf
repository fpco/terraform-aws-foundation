variable "group_name" {
  description = "The name of the group"
  type        = string
}

variable "members" {
  description = "List of strings, where each entry is the username for a user in IAM that should be a member of the group"
  type        = list(string)
}

variable "path" {
  default     = "/"
  description = "string path in IAM, for the users created (all get the same path)"
  type        = string
}

variable "policy_arns" {
  default     = []
  description = "List of strings, where each entry is the name of an IAM policy to attach to the group"
  type        = list(string)
}

resource "aws_iam_group" "g" {
  name  = var.group_name
}

### The list of users are each members of this group
resource "aws_iam_group_membership" "main" {
  group = aws_iam_group.g.name
  name  = var.group_name
  users = var.members
}

resource "aws_iam_group_policy_attachment" "main" {
  count      = length(var.policy_arns)
  group      = aws_iam_group.g.name
  policy_arn = var.policy_arns[count.index]
}

output "group" {
  description = "The IAM group object"
  value       = aws_iam_group.g
}
