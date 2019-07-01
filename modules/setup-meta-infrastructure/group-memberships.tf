#----------------------------------------------------------------------
# Group memberships
#----------------------------------------------------------------------

resource "aws_iam_group_membership" "admin" {
  count = var.create_groups
  name  = "${var.admin_group_name}-membership"
  users = var.admin_group_members
  group = aws_iam_group.admin[0].name
}

resource "aws_iam_group_membership" "power-user" {
  count = var.create_groups
  name  = "${var.power_user_group_name}-membership"
  users = var.power_user_group_members
  group = aws_iam_group.power-user[0].name
}

resource "aws_iam_group_membership" "setup-mfa" {
  count = var.create_groups
  name  = "setup-mfa-membership"
  users = var.setup_mfa_group_members
  group = aws_iam_group.setup-mfa[0].name
}

