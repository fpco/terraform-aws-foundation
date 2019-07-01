#----------------------------------------------------------------------
# Role policy attachments
#----------------------------------------------------------------------

# We use the '*-no-mfa' because that works more reliably after assuming
# a role.  The assume-role-policies require MFA anyway, so this is still
# secure.

resource "aws_iam_role_policy_attachment" "admin_admin" {
  role       = module.admin-role.name
  policy_arn = aws_iam_policy.admin-no-mfa.arn
}

resource "aws_iam_role_policy_attachment" "power-user_power-user" {
  role       = module.power-user-role.name
  policy_arn = aws_iam_policy.power-user-no-mfa.arn
}

