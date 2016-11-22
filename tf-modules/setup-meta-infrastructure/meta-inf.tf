#======================================================================
# meta-inf.tf
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

data "template_file" "admin-group-policy" {
  template = "${file("${path.module}/admin-group-policy-doc.json.tpl")}"
  vars {
    account_id = "${data.aws_caller_identity.current.account_id}"
  }
}

resource "aws_iam_group_policy" "admin" {
  name = "admin"
  group = "${aws_iam_group.admin.name}"

  # Has full access to everything, including IAM management.  Requires MFA.
  policy = "${data.template_file.admin-group-policy.rendered}"
}

data "template_file" "power-user-group-policy" {
  template = "${file("${path.module}/power-user-group-policy-doc.json.tpl")}"
  vars {
    account_id = "${data.aws_caller_identity.current.account_id}"
  }
}

resource "aws_iam_group_policy" "power-user" {
  name = "power-user"
  group = "${aws_iam_group.power-user.name}"

  # Has full access to AWS, _except_ for IAM management (besides their own user,
  # e.g. to change their own password). Requires MFA.
  policy = "${data.template_file.power-user-group-policy.rendered}"
}

data "template_file" "setup-mfa-group-policy" {
  template = "${file("${path.module}/setup-mfa-group-policy-doc.json.tpl")}"
  vars {
    account_id = "${data.aws_caller_identity.current.account_id}"
  }
}

resource "aws_iam_group_policy" "setup-mfa" {
  name = "setup-mfa"
  group = "${aws_iam_group.setup-mfa.id}"

  # This group _only_ allows a user to set up their MFA device (and does not require
  # MFA to log in). If a user belongs to this group, they should not belong to any
  # other groups (otherwise, MFA is pointless since anyone with their credentials
  # could remove their MFA device).
  policy = "${data.template_file.setup-mfa-group-policy.rendered}"
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
