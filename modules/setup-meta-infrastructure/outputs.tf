output "admin-group-name" {
  value       = aws_iam_group.admin.*.name
  description = "`name` exported from the `admin` `aws_iam_group`"
}

output "power-user-group-name" {
  value       = aws_iam_group.power-user.*.name
  description = "`name` exported from the `power-user` `aws_iam_group`"
}

output "setup-mfa-group-name" {
  value       = aws_iam_group.setup-mfa.*.name
  description = "`name` exported from the `setup-mfa` `aws_iam_group`"
}

output "admin-with-mfa-policy-arn" {
  value = aws_iam_policy.admin-with-mfa.arn
}

output "admin-no-mfa-policy-arn" {
  value = aws_iam_policy.admin-no-mfa.arn
}

output "assume-admin-role-policy-arn" {
  value = module.assume-admin-role-policy.arn
}

output "power-user-with-mfa-policy-arn" {
  value = aws_iam_policy.power-user-with-mfa.arn
}

output "power-user-no-mfa-policy-arn" {
  value = aws_iam_policy.power-user-no-mfa.arn
}

output "assume-power-user-role-policy-arn" {
  value = module.assume-power-user-role-policy.arn
}

output "setup-mfa-policy-arn" {
  value = aws_iam_policy.setup-mfa.arn
}

output "manage-own-credentials-with-mfa-policy-arn" {
  value = aws_iam_policy.manage-own-credentials-with-mfa.arn
}

output "admin-role-arn" {
  value = module.admin-role.arn
}

output "power-user-role-arn" {
  value = module.power-user-role.arn
}

