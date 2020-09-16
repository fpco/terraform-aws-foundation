output "json" {
  value       = data.aws_iam_policy_document.vault_iam_permissions.json
  description = ""
}
