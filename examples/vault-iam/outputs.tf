output "secret_key" {
  value = [aws_iam_access_key.vault_iam_admin_key.*.secret]
}

output "access_id" {
  value = [aws_iam_access_key.vault_iam_admin_key.*.id]
}

