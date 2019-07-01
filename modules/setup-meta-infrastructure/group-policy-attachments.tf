#----------------------------------------------------------------------
# Group policy attachments
#----------------------------------------------------------------------

resource "aws_iam_group_policy_attachment" "admin_admin" {
  count      = 1 - var.groups_require_assume_role * var.create_groups
  group      = aws_iam_group.admin[0].name
  policy_arn = aws_iam_policy.admin-with-mfa.arn
}

resource "aws_iam_group_policy_attachment" "admin_assume-admin-role" {
  count      = var.create_groups
  group      = aws_iam_group.admin[0].name
  policy_arn = module.assume-admin-role-policy.arn
}

resource "aws_iam_group_policy_attachment" "admin_assume-power-user-role" {
  count      = var.create_groups
  group      = aws_iam_group.admin[0].name
  policy_arn = module.assume-power-user-role-policy.arn
}

resource "aws_iam_group_policy_attachment" "admin_manage-own-credentials-with-mfa" {
  count      = var.create_groups
  group      = aws_iam_group.admin[0].name
  policy_arn = aws_iam_policy.manage-own-credentials-with-mfa.arn
}

resource "aws_iam_group_policy_attachment" "power-user_power-user" {
  count      = 1 - var.groups_require_assume_role * var.create_groups
  group      = aws_iam_group.power-user[0].name
  policy_arn = aws_iam_policy.power-user-with-mfa.arn
}

resource "aws_iam_group_policy_attachment" "power-user_manage-own-credentials-with-mfa" {
  count      = var.create_groups
  group      = aws_iam_group.power-user[0].name
  policy_arn = aws_iam_policy.manage-own-credentials-with-mfa.arn
}

resource "aws_iam_group_policy_attachment" "power-user_assume-power-user-role" {
  count      = var.create_groups
  group      = aws_iam_group.power-user[0].name
  policy_arn = module.assume-power-user-role-policy.arn
}

resource "aws_iam_group_policy_attachment" "setup-mfa_setup-mfa" {
  count      = var.create_groups
  group      = aws_iam_group.setup-mfa[0].name
  policy_arn = aws_iam_policy.setup-mfa.arn
}

