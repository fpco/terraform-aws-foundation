/**
 * ## Scale Auto-Scaling Groups
 *
 * This module is incomplete.
 *
 */
variable "name" {
  description = "name for resources"
  type        = string
}

resource "aws_iam_policy" "main" {
  name        = "${var.name}-scale-all-asgs"
  description = "allows users to scale up/down any ASGs in the account"
  policy      = data.aws_iam_policy_document.policy.json
}

data "aws_iam_policy_document" "policy" {
  statement {
    effect = "Allow"

    actions = [
      "autoscaling:SetDesiredCapacity",
      "autoscaling:DescribeAutoScalingGroups",
    ]

    resources = ["*"]
  }
}

output "id" {
  value = aws_iam_policy.main.id
}

output "arn" {
  value = aws_iam_policy.main.arn
}

output "name" {
  value = aws_iam_policy.main.name
}

