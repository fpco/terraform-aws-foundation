output "workspace_aws_access_key_id" {
  value       = tfe_variable.workspace_aws_access_key_id.id
  description = "Access key tfe_variable id"
}

output "workspace_aws_secret_access_key_id" {
  value       = tfe_variable.workspace_aws_secret_access_key_id.id
  description = "Access secret tfe_variable id"
}

output "workspace_aws_default_region" {
  value       = tfe_variable.workspace_aws_default_region.id
  description = "Region tfe_variable id"
}
