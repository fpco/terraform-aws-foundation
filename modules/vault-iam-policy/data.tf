data "aws_caller_identity" "current" {}

locals {
 arn_aws_iam_account = "arn:aws:iam::${data.aws_caller_identity.current.account_id}"
}

# The permissions granted to the IAM user/role setup for Vault
data "aws_iam_policy_document" "vault_iam_permissions" {
  statement {
    actions = [
        "iam:AttachUserPolicy",
        "iam:CreateAccessKey",
        "iam:CreateUser",
        "iam:DeleteAccessKey",
        "iam:DeleteUser",
        "iam:DeleteUserPolicy",
        "iam:DetachUserPolicy",
        "iam:ListAccessKeys",
        "iam:ListAttachedUserPolicies",
        "iam:ListGroupsForUser",
        "iam:ListUserPolicies",
        "iam:PutUserPolicy",
        "iam:RemoveUserFromGroup"
    ]

    resources = [
        "${local.arn_aws_iam_account}:user/${var.vault_iam_container_name}-*"
    ]

  }
}
