/**
 * ## Cross Account Group
 *
 * Write docs.
 *
 */

variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}

variable "name" {
  description = "name of the group"
}

variable "mfa_policy_arn" {
  description = "ARN of the self-managed MFA policy"
}

variable "account_ids" {
  type        = "list"
  description = "list of AWS account IDs (ARNs) to link to the group"
}

# check with manny on the ...
variable "role_name" {
  description = "name of the ... role, OK to leave this as default"
  default     = "power-user"
}

variable "users" {
  type        = "list"
  description = "list of users who have access to this group/role"
}

resource "aws_iam_group" "main" {
  name = "${var.name}"
}

# check with manny on the name of this resource
resource "aws_iam_group_policy_attachment" "manage-own-credentials-with-mfa" {
  group      = "${aws_iam_group.main.name}"
  policy_arn = "${var.mfa_policy_arn}"
}

module "assume-group-role-policy" {
  source      = "../cross-account-assume-role-policy"
  aws_cloud   = "${var.aws_cloud}"
  policy_name = "assume-${var.name}-${var.role_name}-role"
  role_name   = "${var.role_name}"
  account_ids = ["${var.account_ids}"]
}

resource "aws_iam_group_policy_attachment" "group-assume-role" {
  group      = "${aws_iam_group.main.name}"
  policy_arn = "${module.assume-group-role-policy.arn}"
}

resource "aws_iam_group_membership" "main" {
  name  = "${var.name}-users"
  users = ["${compact(var.users)}"]
  group = "${aws_iam_group.main.name}"
}
