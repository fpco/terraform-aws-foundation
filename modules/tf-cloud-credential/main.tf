resource "tfe_variable" "workspace_aws_access_key_id" {
  workspace_id = var.workspace_id
  key          = "AWS_ACCESS_KEY_ID"
  value        = var.iam_access_key.id
  category     = "env"
  sensitive    = true
}

resource "tfe_variable" "workspace_aws_secret_access_key_id" {
  workspace_id = var.workspace_id
  key          = "AWS_SECRET_ACCESS_KEY"
  value        = var.iam_access_key.secret
  category     = "env"
  sensitive    = true
}

resource "tfe_variable" "workspace_aws_default_region" {
  workspace_id = var.workspace_id
  key          = "AWS_DEFAULT_REGION"
  value        = var.region
  category     = "env"
  sensitive    = false
}
