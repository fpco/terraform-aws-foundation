#======================================================================
# iam.tf
# Meta-infrastructure - IAM users, groups, etc. not specific to the
# product itself.
#======================================================================

#----------------------------------------------------------------------
# Outputs
#----------------------------------------------------------------------

output "admin-group-name" {
  value = "${aws_iam_group.admin.name}"
}

output "power-user-group-name" {
  value = "${aws_iam_group.power-user.name}"
}

output "setup-mfa-group-name" {
  value = "${aws_iam_group.setup-mfa.name}"
}

#----------------------------------------------------------------------
# Data sources
#----------------------------------------------------------------------

data "aws_caller_identity" "current" { }

#----------------------------------------------------------------------
# Groups
#----------------------------------------------------------------------

resource "aws_iam_group" "admin" {
  name = "admin"
}

resource "aws_iam_group" "power-user" {
  name = "power-user"
}

resource "aws_iam_group" "setup-mfa" {
  name = "setup-mfa"
}

#----------------------------------------------------------------------
# Policies and attachments
#----------------------------------------------------------------------

# Has full access to everything, including IAM management.  Requires MFA.
data "aws_iam_policy_document" "admin" {
  statement {
    effect = "Allow"
    actions = [
      "*",
      "aws-portal:*",
      "support:*"]
    resources = ["*"]
    condition {
      test = "Bool"
      variable = "aws:MultiFactorAuthPresent"
      values = ["true"]
    }
  }
  statement {
    effect = "Allow",
    actions = ["iam:ListMFADevices"]
    resources = ["*"]
  }
}

resource "aws_iam_group_policy" "admin" {
  name = "admin"
  group = "${aws_iam_group.admin.name}"
  policy = "${data.aws_iam_policy_document.admin.json}"
}

# Has full access to AWS, _except_ for IAM management (besides their own user,
# e.g. to change their own password). Requires MFA.
data "aws_iam_policy_document" "power-user" {
  statement {
    effect = "Allow"
    not_actions = ["iam:*"]
    resources = ["*"]
    condition {
      test = "Bool"
      variable = "aws:MultiFactorAuthPresent"
      values = ["true"]
    }
  }
  statement {
    effect = "Allow"
    actions = [
      "iam:*LoginProfile",
      "iam:*AccessKey*"
    ]
    resources = ["arn:aws:iam::${data.aws_caller_identity.current.account_id}:user/&{aws:username}"]
    condition {
      test = "Bool"
      variable = "aws:MultiFactorAuthPresent"
      values = ["true"]
    }
  }
  statement {
    effect = "Allow"
    actions = [
      "iam:ListAccount*",
      "iam:GetAccount*",
      "iam:ListUsers",
      "iam:ListRoles"
    ],
    resources = ["*"]
    condition {
      test = "Bool"
      variable = "aws:MultiFactorAuthPresent"
      values = ["true"]
    }
  }
  statement {
    effect = "Allow"
    actions = ["iam:ListMFADevices"]
    resources = ["*"]
  }
}

resource "aws_iam_group_policy" "power-user" {
  name = "power-user"
  group = "${aws_iam_group.power-user.name}"
  policy = "${data.aws_iam_policy_document.power-user.json}"
}

# Only allows a user to set up their MFA device (and does not require
# MFA to log in). If a user belongs to this group, they should not belong to any
# other groups (otherwise, MFA is pointless since anyone with their credentials
# could remove their MFA device).
data "aws_iam_policy_document" "setup-mfa" {
  statement {
    sid = "AllowUsersToCreateDeleteTheirOwnVirtualMFADevices"
    effect = "Allow"
    actions = ["iam:*VirtualMFADevice"]
    resources = ["arn:aws:iam::${data.aws_caller_identity.current.account_id}:mfa/&{aws:username}"]
  }
  statement {
    sid = "AllowUsersToEnableSyncDisableTheirOwnMFADevices"
    effect = "Allow"
    actions = [
      "iam:DeactivateMFADevice",
      "iam:EnableMFADevice",
      "iam:ListMFADevices",
      "iam:ResyncMFADevice"
    ]
    resources = ["arn:aws:iam::${data.aws_caller_identity.current.account_id}:user/&{aws:username}"]
  }
  statement {
    sid = "AllowUsersToListVirtualMFADevices"
    effect = "Allow"
    actions = ["iam:ListVirtualMFADevices"]
    resources = ["arn:aws:iam::${data.aws_caller_identity.current.account_id}:mfa/*"]
  }
  statement {
    sid = "AllowUsersToListUsersInConsole"
    effect = "Allow"
    actions = ["iam:ListUsers"]
    resources = ["arn:aws:iam::${data.aws_caller_identity.current.account_id}:user/*"]
  }
}

resource "aws_iam_group_policy" "setup-mfa" {
  name = "setup-mfa"
  group = "${aws_iam_group.setup-mfa.id}"
  policy = "${data.aws_iam_policy_document.setup-mfa.json}"
}

#----------------------------------------------------------------------
# Password policy
#----------------------------------------------------------------------

resource "aws_iam_account_password_policy" "default-password-policy" {
  minimum_password_length        = 12
  max_password_age               = 90
  require_lowercase_characters   = true
  require_numbers                = true
  require_uppercase_characters   = true
  require_symbols                = true
  allow_users_to_change_password = true
}
