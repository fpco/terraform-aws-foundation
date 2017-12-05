/**
 * ## Cross-Account Assume Role Policy
 *
 * Creates an IAM policy that allows attached entities to assume given role(s)
 * in given account(s).
 *
 * Either `account_ids` and `role_name` can be provided, OR `account_id` and
 * `role_names`. Other combinations will not work correctly due to limitations
 * in Terraform.
 *
 */

variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}

variable "policy_name" {
  description = "Name of the policy."
}

variable "account_ids" {
  description = "List of accounts in which the role specified by 'role_name' can be assumed.  Only for use in combination with 'role_name'."
  default = []
}

variable "role_name" {
  description = "The role that can be assumed in the accounts given in 'account_ids'.  Only for use in combination with 'account_ids'."
  default = ""
}

variable "account_id" {
  description = "The account that can assume the roles specified by 'role_names'.  Only for use in combination with 'role_names'."
  default = ""
}

variable "role_names" {
  description = "List of roles that can be assumed in the account specified in 'account_id'.  Only for use in combination with 'account_id'."
  default = []
}

output "arn" {
  value = "${aws_iam_policy.assume-role.arn}"
}

data "aws_iam_policy_document" "assume-role" {
  statement {
    effect = "Allow"
    actions = ["sts:AssumeRole"]
    resources = [
      "${formatlist("arn:${var.aws_cloud}:iam::%s:role/%s",var.account_ids,var.role_name)}",
      "${formatlist("arn:${var.aws_cloud}:iam::%s:role/%s",var.account_id,var.role_names)}"
    ]
  }
}

resource "aws_iam_policy" "assume-role" {
  name = "${var.policy_name}"
  policy = "${data.aws_iam_policy_document.assume-role.json}"
}
