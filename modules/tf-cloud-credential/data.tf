data "tfe_workspace" "workspace" {
  name         = var.name_prefix
  organization = var.organization
}
