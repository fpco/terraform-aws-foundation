#======================================================================
# meta-inf.tf
# Meta-infrastructure - IAM users, groups, etc. not specific to the
# product itself.
#======================================================================

#----------------------------------------------------------------------
# Users
#----------------------------------------------------------------------

# Users should generate their own keys in the console.
# Therefore, we have force_destroy here to destroy those
# unmanaged keys on user destruction

resource "aws_iam_user" "tadams" {
  name = "tadams"
  force_destroy = true
}

#----------------------------------------------------------------------
# Groups
#----------------------------------------------------------------------

resource "aws_iam_group" "admin" {
  name = "admin"
}

resource "aws_iam_group" "setup-mfa" {
  name = "setup-mfa"
}

#----------------------------------------------------------------------
# Policies and attachments
#----------------------------------------------------------------------

resource "aws_iam_group_policy" "setup-mfa-policy" {
  name = "setup-mfa-policy"
  group = "${aws_iam_group.setup-mfa.id}"

  # allows a user to access the IAM interface in the console and dig into
  # their own user in order to setup MFA, and allows them to change their
  # password as will be required on first login
  policy = "${file("setup-mfa-group-policy-doc.json")}"
}

resource "aws_iam_group_policy_attachment" "admin-group-policy-attachment" {
  group = "${aws_iam_group.admin.name}"
  policy_arn = "arn:aws:iam::aws:policy/AdministratorAccess" # AWS managed policy
}

#----------------------------------------------------------------------
# Group membership
#----------------------------------------------------------------------

resource "aws_iam_group_membership" "admin-membership" {
  name = "admin-membership"
  users = [
    "${aws_iam_user.tadams.name}",
  ]
  group = "${aws_iam_group.admin.name}"
}

resource "aws_iam_group_membership" "setup-mfa-membership" {
  name = "setup-mfa-membership"
  users = [
  ]
  group = "${aws_iam_group.setup-mfa.name}"
}

#----------------------------------------------------------------------
# Password policy
#----------------------------------------------------------------------

resource "aws_iam_account_password_policy" "default-password-policy" {
  minimum_password_length = 12

  require_lowercase_characters   = true
  require_numbers                = true
  require_uppercase_characters   = true
  require_symbols                = true
  allow_users_to_change_password = true
}
