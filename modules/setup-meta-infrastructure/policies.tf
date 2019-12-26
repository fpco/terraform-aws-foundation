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
      values   = [true]
    }
  }
}

resource "aws_iam_policy" "admin-with-mfa" {
  name   = "full-administrator-access-with-mfa"
  policy = data.aws_iam_policy_document.admin-with-mfa.json
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
  policy = data.aws_iam_policy_document.admin-no-mfa.json
}

module "assume-admin-role-policy" {
  source      = "../cross-account-assume-role-policy"
  policy_name = "assume-control-accounts-admin-role"
  role_name   = module.admin-role.name

  account_ids = concat([data.aws_caller_identity.current.account_id],
    var.admin_control_account_ids)
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
      values   = [true]
    }
  }

  statement {
    effect = "Allow"

    actions = [
      "iam:GetGroup",
      "iam:GetGroupPolicy",
      "iam:GetInstanceProfile",
      "iam:GetLoginProfile",
      "iam:GetOpenIDConnectProvider",
      "iam:GetPolicy",
      "iam:GetPolicyVersion",
      "iam:GetRole",
      "iam:GetRolePolicy",
      "iam:GetServerCertificate",
      "iam:GetUser",
      "iam:GetUserPolicy",
      "iam:ListAttachedGroupPolicies",
      "iam:ListAttachedRolePolicies",
      "iam:ListAttachedUserPolicies",
      "iam:ListEntitiesForPolicy",
      "iam:ListGroupPolicies",
      "iam:ListGroups",
      "iam:ListGroupsForUser",
      "iam:ListInstanceProfiles",
      "iam:ListInstanceProfilesForRole",
      "iam:ListOpenIDConnectProviders",
      "iam:ListPolicies",
      "iam:ListPoliciesGrantingServiceAccess",
      "iam:ListPolicyVersions",
      "iam:ListRolePolicies",
      "iam:ListRoles",
      "iam:ListRoleTags",
      "iam:ListServerCertificates",
      "iam:ListServiceSpecificCredentials",
      "iam:ListSigningCertificates",
      "iam:ListUserPolicies",
      "iam:ListUsers",
      "iam:ListUserTags",
      "organizations:DescribeOrganization",
    ]

    resources = ["*"]

    condition {
      test     = "Bool"
      variable = "aws:MultiFactorAuthPresent"
      values   = [true]
    }
  }
}

resource "aws_iam_policy" "power-user-with-mfa" {
  name   = "power-user-access-with-mfa"
  policy = data.aws_iam_policy_document.power-user-with-mfa.json
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
      "iam:GetGroup",
      "iam:GetGroupPolicy",
      "iam:GetInstanceProfile",
      "iam:GetLoginProfile",
      "iam:GetOpenIDConnectProvider",
      "iam:GetPolicy",
      "iam:GetPolicyVersion",
      "iam:GetRole",
      "iam:GetRolePolicy",
      "iam:GetServerCertificate",
      "iam:GetUser",
      "iam:GetUserPolicy",
      "iam:ListAttachedGroupPolicies",
      "iam:ListAttachedRolePolicies",
      "iam:ListAttachedUserPolicies",
      "iam:ListEntitiesForPolicy",
      "iam:ListGroupPolicies",
      "iam:ListGroups",
      "iam:ListGroupsForUser",
      "iam:ListInstanceProfiles",
      "iam:ListInstanceProfilesForRole",
      "iam:ListOpenIDConnectProviders",
      "iam:ListPolicies",
      "iam:ListPoliciesGrantingServiceAccess",
      "iam:ListPolicyVersions",
      "iam:ListRolePolicies",
      "iam:ListRoles",
      "iam:ListRoleTags",
      "iam:ListServerCertificates",
      "iam:ListServiceSpecificCredentials",
      "iam:ListSigningCertificates",
      "iam:ListUserPolicies",
      "iam:ListUsers",
      "iam:ListUserTags",
      "organizations:DescribeOrganization",
    ]

    resources = ["*"]
  }
}

resource "aws_iam_policy" "power-user-no-mfa" {
  name   = "power-user-access-no-mfa"
  policy = data.aws_iam_policy_document.power-user-no-mfa.json
}

module "assume-power-user-role-policy" {
  source      = "../cross-account-assume-role-policy"
  policy_name = "assume-control-accounts-power-user-role"
  role_name   = module.power-user-role.name

  account_ids = concat([data.aws_caller_identity.current.account_id],
    var.power_user_control_account_ids)
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
    resources = ["arn:${data.aws_partition.current.partition}:iam::${data.aws_caller_identity.current.account_id}:mfa/&{aws:username}"]
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

    resources = ["arn:${data.aws_partition.current.partition}:iam::${data.aws_caller_identity.current.account_id}:user/&{aws:username}"]
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
    resources = ["arn:${data.aws_partition.current.partition}:iam::${data.aws_caller_identity.current.account_id}:mfa/*"]
  }

  statement {
    sid       = "AllowUsersToListUsersInConsole"
    effect    = "Allow"
    actions   = ["iam:ListUsers"]
    resources = ["arn:${data.aws_partition.current.partition}:iam::${data.aws_caller_identity.current.account_id}:user/*"]
  }
}

resource "aws_iam_policy" "setup-mfa" {
  name   = "setup-mfa-device"
  policy = data.aws_iam_policy_document.setup-mfa.json
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

    resources = ["arn:${data.aws_partition.current.partition}:iam::${data.aws_caller_identity.current.account_id}:user/&{aws:username}"]

    condition {
      test     = "Bool"
      variable = "aws:MultiFactorAuthPresent"
      values   = [true]
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
      values   = [true]
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
  policy = data.aws_iam_policy_document.manage-own-credentials-with-mfa.json
}

