resource "tfe_workspace" "workspace" {
  name         = var.workspace_name_prefix
  organization = var.organization
}
