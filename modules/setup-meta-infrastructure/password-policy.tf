#----------------------------------------------------------------------
# Password policy
#----------------------------------------------------------------------

resource "aws_iam_account_password_policy" "default-password-policy" {
  count                          = var.set_password_policy
  minimum_password_length        = 12
  max_password_age               = 90
  require_lowercase_characters   = true
  require_numbers                = true
  require_uppercase_characters   = true
  require_symbols                = true
  allow_users_to_change_password = true
}

