output "secret_key" {
  value = [aws_iam_access_key.vaultkey.*.secret]
}

output "access_id" {
  value = [aws_iam_access_key.vaultkey.*.id]
}

