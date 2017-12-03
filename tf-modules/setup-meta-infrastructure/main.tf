/**
 * ## MFA and IAM
 *
 * This module provides a "boxed" set of IAM groups and policies suitable
 * for managing account access through IAM and leveraging MFA.
 *
 * NOTE: need to document each group, policy, and the resulting permissions
 *
 */

#----------------------------------------------------------------------
# Variables
#----------------------------------------------------------------------

variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}

variable "create_groups" {
  description = "Set to 0 to disable creating the 'admin', 'power-user', and 'setup-mfa' groups.  This should be done for accounts that users do not sign into directly (only delegated access)."
  default     = 1
}

variable "groups_require_assume_role" {
  description = "Set to 1 to give the 'admin' and 'power-user' groups no privileges aside from being able to assume roles.  This is a best practice and recommended for all new accounts.  Only relevant if create_groups = 1."
  default     = 0
}

variable "trust_account_ids" {
  description = "A list of accounts that are trusted.  Trusted accounts can assume the 'admin' and 'power-user' roles in the current account."
  default     = []
}

variable "admin_control_account_ids" {
  description = "List of accounts that users in the 'admin' group can assume the 'admin' and 'power-user' roles in.  These accounts must trust the current account.  Only relevant if create_groups = 1."
  default     = []
}

variable "power_user_control_account_ids" {
  description = "List of accounts that users in the 'power-user' group can assume the 'power-user' role in.  These accounts must trust the current account.  Only relevant if create_groups = 1."
  default     = []
}

variable "admin_group_name" {
  description = "The name of the administrators group.  A common value for new accounts is 'super-admin', since these admins have control over other accounts.  Only relevant if create_groups = 1."
  default     = "admin"
}

variable "power_user_group_name" {
  description = "The name of the power users group.  A common value for new accounts with delegation is 'super-power-user', since these admins have control over other accounts.  Only relevant if create_groups = 1."
  default     = "power-user"
}

variable "admin_group_members" {
  description = "Users in the 'admin' group.  Only relevant if create_groups = 1."
  default     = []
}

variable "power_user_group_members" {
  description = "Users in the 'power-user' group.  Only relevant if create_groups = 1."
  default     = []
}

variable "setup_mfa_group_members" {
  description = "Users in the 'setup-mfa' group.  Only relevant if create_groups = 1."
  default     = []
}

variable "set_password_policy" {
  description = "Set to 0 to disable setting the account password policy"
  default     = 1
}

#----------------------------------------------------------------------
# Outputs
#----------------------------------------------------------------------

//`name` exported from the `admin` `aws_iam_group`
output "admin-group-name" {
  value = "${aws_iam_group.admin.name}"
}

//`name` exported from the `power-user` `aws_iam_group`
output "power-user-group-name" {
  value = "${aws_iam_group.power-user.name}"
}

//`name` exported from the `setup-mfa` `aws_iam_group`
output "setup-mfa-group-name" {
  value = "${aws_iam_group.setup-mfa.name}"
}

output "admin-with-mfa-policy-arn" {
  value = "${aws_iam_policy.admin-with-mfa.arn}"
}

output "admin-no-mfa-policy-arn" {
  value = "${aws_iam_policy.admin-no-mfa.arn}"
}

output "assume-admin-role-policy-arn" {
  value = "${module.assume-admin-role-policy.arn}"
}

output "power-user-with-mfa-policy-arn" {
  value = "${aws_iam_policy.power-user-with-mfa.arn}"
}

output "power-user-no-mfa-policy-arn" {
  value = "${aws_iam_policy.power-user-no-mfa.arn}"
}

output "assume-power-user-role-policy-arn" {
  value = "${module.assume-power-user-role-policy.arn}"
}

output "setup-mfa-policy-arn" {
  value = "${aws_iam_policy.setup-mfa.arn}"
}

output "manage-own-credentials-with-mfa-policy-arn" {
  value = "${aws_iam_policy.manage-own-credentials-with-mfa.arn}"
}

output "admin-role-arn" {
  value = "${module.admin-role.arn}"
}

output "power-user-role-arn" {
  value = "${module.power-user-role.arn}"
}

#----------------------------------------------------------------------
# Data sources
#----------------------------------------------------------------------

data "aws_caller_identity" "current" {}

#----------------------------------------------------------------------
# Groups
#----------------------------------------------------------------------

resource "aws_iam_group" "admin" {
  count = "${var.create_groups}"
  name  = "${var.admin_group_name}"
}

resource "aws_iam_group" "power-user" {
  count = "${var.create_groups}"
  name  = "${var.power_user_group_name}"
}

resource "aws_iam_group" "setup-mfa" {
  count = "${var.create_groups}"
  name  = "setup-mfa"
}

#----------------------------------------------------------------------
# Roles
#----------------------------------------------------------------------

module "admin-role" {
  source    = "../cross-account-role"
  aws_cloud = "${var.aws_cloud}"
  name      = "admin"

  trust_account_ids = [
    "${data.aws_caller_identity.current.account_id}",
    "${var.trust_account_ids}",
  ]
}

module "power-user-role" {
  source    = "../cross-account-role"
  aws_cloud = "${var.aws_cloud}"
  name      = "power-user"

  trust_account_ids = [
    "${data.aws_caller_identity.current.account_id}",
    "${var.trust_account_ids}",
  ]
}

#----------------------------------------------------------------------
# Policies
#----------------------------------------------------------------------

# Has full access to everything, including IAM management.  Requires MFA.
data "aws_iam_policy_document" "admin-with-mfa" {
  statement {
    effect = "Allow"

    actions = [
      "*",
      "aws-portal:*",
      "support:*",
    ]

    resources = ["*"]

    condition {
      test     = "Bool"
      variable = "aws:MultiFactorAuthPresent"
      values   = ["true"]
    }
  }
}

resource "aws_iam_policy" "admin-with-mfa" {
  name   = "full-administrator-access-with-mfa"
  policy = "${data.aws_iam_policy_document.admin-with-mfa.json}"
}

# Has full access to everything, including IAM management.  Does NOT require MFA.
data "aws_iam_policy_document" "admin-no-mfa" {
  statement {
    effect = "Allow"

    actions = [
      "*",
      "aws-portal:*",
      "support:*",
    ]

    resources = ["*"]
  }
}

resource "aws_iam_policy" "admin-no-mfa" {
  name   = "full-administrator-access-no-mfa"
  policy = "${data.aws_iam_policy_document.admin-no-mfa.json}"
}

module "assume-admin-role-policy" {
  source      = "../cross-account-assume-role-policy"
  aws_cloud   = "${var.aws_cloud}"
  policy_name = "assume-control-accounts-admin-role"
  role_name   = "${module.admin-role.name}"

  account_ids = [
    "${data.aws_caller_identity.current.account_id}",
    "${var.admin_control_account_ids}",
  ]
}

# Has full access to AWS, _except_ for IAM management. Requires MFA.
data "aws_iam_policy_document" "power-user-with-mfa" {
  statement {
    effect = "Allow"

    not_actions = [
      "iam:*",
      "organizations:*",
    ]

    resources = ["*"]

    condition {
      test     = "Bool"
      variable = "aws:MultiFactorAuthPresent"
      values   = ["true"]
    }
  }

  statement {
    effect = "Allow"

    actions = [
      "iam:ListAccount*",
      "iam:GetAccount*",
      "iam:ListUsers",
      "iam:ListRoles",
      "organizations:DescribeOrganization",
    ]

    resources = ["*"]

    condition {
      test     = "Bool"
      variable = "aws:MultiFactorAuthPresent"
      values   = ["true"]
    }
  }
}

resource "aws_iam_policy" "power-user-with-mfa" {
  name   = "power-user-access-with-mfa"
  policy = "${data.aws_iam_policy_document.power-user-with-mfa.json}"
}

# Has full access to AWS, _except_ for IAM management. Does NOT require MFA.
data "aws_iam_policy_document" "power-user-no-mfa" {
  statement {
    effect = "Allow"

    not_actions = [
      "iam:*",
      "organizations:*",
    ]

    resources = ["*"]
  }

  statement {
    effect = "Allow"

    actions = [
      "iam:ListAccount*",
      "iam:GetAccount*",
      "iam:ListUsers",
      "iam:ListRoles",
      "organizations:DescribeOrganization",
    ]

    resources = ["*"]
  }
}

resource "aws_iam_policy" "power-user-no-mfa" {
  name   = "power-user-access-no-mfa"
  policy = "${data.aws_iam_policy_document.power-user-no-mfa.json}"
}

module "assume-power-user-role-policy" {
  source      = "../cross-account-assume-role-policy"
  aws_cloud   = "${var.aws_cloud}"
  policy_name = "assume-control-accounts-power-user-role"
  role_name   = "${module.power-user-role.name}"

  account_ids = [
    "${data.aws_caller_identity.current.account_id}",
    "${var.power_user_control_account_ids}",
  ]
}

# Only allows a user to set up their MFA device (and does not require
# MFA to log in). If a user belongs to this group, they should not belong to any
# other groups (otherwise, MFA is pointless since anyone with their credentials
# could remove their MFA device).
data "aws_iam_policy_document" "setup-mfa" {
  statement {
    sid       = "AllowUsersToCreateDeleteTheirOwnVirtualMFADevices"
    effect    = "Allow"
    actions   = ["iam:*VirtualMFADevice"]
    resources = ["arn:${var.aws_cloud}:iam::${data.aws_caller_identity.current.account_id}:mfa/&{aws:username}"]
  }

  statement {
    sid    = "AllowUsersToEnableSyncDisableTheirOwnMFADevices"
    effect = "Allow"

    actions = [
      "iam:DeactivateMFADevice",
      "iam:EnableMFADevice",
      "iam:ListMFADevices",
      "iam:ResyncMFADevice",
      "iam:ChangePassword",
    ]

    resources = ["arn:${var.aws_cloud}:iam::${data.aws_caller_identity.current.account_id}:user/&{aws:username}"]
  }

  statement {
    sid       = "AllowUsersToGetAccountPasswordPolicy"
    effect    = "Allow"
    actions   = ["iam:GetAccountPasswordPolicy"]
    resources = ["*"]
  }

  statement {
    sid       = "AllowUsersToListVirtualMFADevices"
    effect    = "Allow"
    actions   = ["iam:ListVirtualMFADevices"]
    resources = ["arn:${var.aws_cloud}:iam::${data.aws_caller_identity.current.account_id}:mfa/*"]
  }

  statement {
    sid       = "AllowUsersToListUsersInConsole"
    effect    = "Allow"
    actions   = ["iam:ListUsers"]
    resources = ["arn:${var.aws_cloud}:iam::${data.aws_caller_identity.current.account_id}:user/*"]
  }
}

resource "aws_iam_policy" "setup-mfa" {
  name   = "setup-mfa-device"
  policy = "${data.aws_iam_policy_document.setup-mfa.json}"
}

# Allow a user to change their password, manage their access keys, and resync their MFA device (but not change it).
data "aws_iam_policy_document" "manage-own-credentials-with-mfa" {
  statement {
    effect = "Allow"

    actions = [
      "iam:*LoginProfile",
      "iam:*AccessKey*",
      "iam:ResyncMFADevice",
      "iam:ChangePassword",
    ]

    resources = ["arn:${var.aws_cloud}:iam::${data.aws_caller_identity.current.account_id}:user/&{aws:username}"]

    condition {
      test     = "Bool"
      variable = "aws:MultiFactorAuthPresent"
      values   = ["true"]
    }
  }

  statement {
    effect = "Allow"

    actions = [
      "iam:ListAccount*",
      "iam:GetAccount*",
      "iam:ListUsers",
      "iam:ListRoles",
      "organizations:DescribeOrganization",
    ]

    resources = ["*"]

    condition {
      test     = "Bool"
      variable = "aws:MultiFactorAuthPresent"
      values   = ["true"]
    }
  }

  statement {
    # This helps tools like https://github.com/lonelyplanet/aws-mfa find out a user's MFA device ARN so that they can prompt for an MFA code.
    effect    = "Allow"
    actions   = ["iam:ListMFADevices"]
    resources = ["*"]
  }
}

resource "aws_iam_policy" "manage-own-credentials-with-mfa" {
  name   = "manage-own-credentials-with-mfa"
  policy = "${data.aws_iam_policy_document.manage-own-credentials-with-mfa.json}"
}

#----------------------------------------------------------------------
# Group policy attachments
#----------------------------------------------------------------------

resource "aws_iam_group_policy_attachment" "admin_admin" {
  count      = "${(1 - var.groups_require_assume_role) * var.create_groups}"
  group      = "${aws_iam_group.admin.name}"
  policy_arn = "${aws_iam_policy.admin-with-mfa.arn}"
}

resource "aws_iam_group_policy_attachment" "admin_assume-admin-role" {
  count      = "${var.create_groups}"
  group      = "${aws_iam_group.admin.name}"
  policy_arn = "${module.assume-admin-role-policy.arn}"
}

resource "aws_iam_group_policy_attachment" "admin_assume-power-user-role" {
  count      = "${var.create_groups}"
  group      = "${aws_iam_group.admin.name}"
  policy_arn = "${module.assume-power-user-role-policy.arn}"
}

resource "aws_iam_group_policy_attachment" "admin_manage-own-credentials-with-mfa" {
  count      = "${var.create_groups}"
  group      = "${aws_iam_group.admin.name}"
  policy_arn = "${aws_iam_policy.manage-own-credentials-with-mfa.arn}"
}

resource "aws_iam_group_policy_attachment" "power-user_power-user" {
  count      = "${(1 - var.groups_require_assume_role) * var.create_groups}"
  group      = "${aws_iam_group.power-user.name}"
  policy_arn = "${aws_iam_policy.power-user-with-mfa.arn}"
}

resource "aws_iam_group_policy_attachment" "power-user_manage-own-credentials-with-mfa" {
  count      = "${var.create_groups}"
  group      = "${aws_iam_group.power-user.name}"
  policy_arn = "${aws_iam_policy.manage-own-credentials-with-mfa.arn}"
}

resource "aws_iam_group_policy_attachment" "power-user_assume-power-user-role" {
  count      = "${var.create_groups}"
  group      = "${aws_iam_group.power-user.name}"
  policy_arn = "${module.assume-power-user-role-policy.arn}"
}

resource "aws_iam_group_policy_attachment" "setup-mfa_setup-mfa" {
  count      = "${var.create_groups}"
  group      = "${aws_iam_group.setup-mfa.name}"
  policy_arn = "${aws_iam_policy.setup-mfa.arn}"
}

#----------------------------------------------------------------------
# Role policy attachments
#----------------------------------------------------------------------

# We use the '*-no-mfa' because that works more reliably after assuming
# a role.  The assume-role-policies require MFA anyway, so this is still
# secure.

resource "aws_iam_role_policy_attachment" "admin_admin" {
  role       = "${module.admin-role.name}"
  policy_arn = "${aws_iam_policy.admin-no-mfa.arn}"
}

resource "aws_iam_role_policy_attachment" "power-user_power-user" {
  role       = "${module.power-user-role.name}"
  policy_arn = "${aws_iam_policy.power-user-no-mfa.arn}"
}

#----------------------------------------------------------------------
# Group memberships
#----------------------------------------------------------------------

resource "aws_iam_group_membership" "admin" {
  count = "${var.create_groups}"
  name  = "${var.admin_group_name}-membership"
  users = ["${var.admin_group_members}"]
  group = "${aws_iam_group.admin.name}"
}

resource "aws_iam_group_membership" "power-user" {
  count = "${var.create_groups}"
  name  = "${var.power_user_group_name}-membership"
  users = ["${var.power_user_group_members}"]
  group = "${aws_iam_group.power-user.name}"
}

resource "aws_iam_group_membership" "setup-mfa" {
  count = "${var.create_groups}"
  name  = "setup-mfa-membership"
  users = ["${var.setup_mfa_group_members}"]
  group = "${aws_iam_group.setup-mfa.name}"
}

#----------------------------------------------------------------------
# Password policy
#----------------------------------------------------------------------

resource "aws_iam_account_password_policy" "default-password-policy" {
  count                          = "${var.set_password_policy}"
  minimum_password_length        = 12
  max_password_age               = 90
  require_lowercase_characters   = true
  require_numbers                = true
  require_uppercase_characters   = true
  require_symbols                = true
  allow_users_to_change_password = true
}
