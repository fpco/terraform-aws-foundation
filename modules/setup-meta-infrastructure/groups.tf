#----------------------------------------------------------------------
# Groups
#----------------------------------------------------------------------

resource "aws_iam_group" "admin" {
  count = var.create_groups
  name  = var.admin_group_name
}

resource "aws_iam_group" "power-user" {
  count = var.create_groups
  name  = var.power_user_group_name
}

resource "aws_iam_group" "setup-mfa" {
  count = var.create_groups
  name  = "setup-mfa"
}

