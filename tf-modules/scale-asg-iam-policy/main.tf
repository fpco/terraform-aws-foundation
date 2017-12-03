/**
 * ## Scale Auto-Scaling Groups
 *
 * This module is incomplete.
 *
 */
variable "name" {}

resource "aws_iam_policy" "main" {
  name        = "${var.name}-scale-all-asgs"
  description = "allows users to scale up/down any ASGs in the account"

  policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "autoscaling:SetDesiredCapacity",
        "autoscaling:DescribeAutoScalingGroups"
      ],
      "Effect": "Allow",
      "Resource": "*"
    }
  ]
}
EOF
}

output "id" {
  value = "${aws_iam_policy.main.id}"
}

output "arn" {
  value = "${aws_iam_policy.main.arn}"
}

output "name" {
  value = "${aws_iam_policy.main.name}"
}
