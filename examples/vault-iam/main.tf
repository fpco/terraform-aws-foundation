# IAM user
module "vault-iam-admin" {
  source                   = "../../modules/vault-iam-admin"
  username                 = var.vault_iam_username
  vault_iam_container_name = var.vault_iam_container_name
}

resource "aws_iam_access_key" "vault_iam_admin_key" {
  user  = module.vault-iam-admin.username
  count = var.key_count
}
