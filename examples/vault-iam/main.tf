resource "aws_iam_access_key" "vaultkey" {
  user  = module.vault_iam_user_policy.user_name
  count = var.key_count
}

module "vault_iam_user_policy" {
  source          = "../../modules/iam-user-policy/"
  user_name       = "vault_user"
  environment     = "dev"
  iam_policy_name = "vault_user_policy"
  iam_user_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
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
      ],
      "Resource": [
        "arn:aws:iam::xxxxxxx:user/vtest-*"
      ]
    }
  ]
}
EOF

}

