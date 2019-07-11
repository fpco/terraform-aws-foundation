resource "aws_iam_user" "vault_user" {
  name = "vault_user"
  tags = {
    "user" = "vault"
  }
}

resource "aws_iam_access_key" "vaultkey" {
  user = "${aws_iam_user.vault_user.name}"
}

resource "aws_iam_user_policy" "vault_user_policy" {
  name = "vault_user_policy"
  user = "${aws_iam_user.vault_user.name}"

  policy = <<EOF
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
        "arn:aws:iam::793514493597:user/vault-*"
      ]
    }
  ]
}
EOF
}
