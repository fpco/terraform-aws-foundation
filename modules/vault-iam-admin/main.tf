locals {
  default_iam_policy_name = "${var.username}-user-policy"
  # TODO conditional here
  iam_policy_name = local.default_iam_policy_name
}

# default IAM policy
module "vault-iam-policy" {
  source                   = "../../modules/vault-iam-policy"
  vault_iam_container_name = var.vault_iam_container_name
}

# TODO add a iam policy datasource here to define the JSON
module "vault-iam-user" {
  source          = "../iam-user-policy/"
  username        = var.username
  extra_tags      = var.extra_tags
  iam_policy_name = local.iam_policy_name
  iam_user_policy = module.vault-iam-policy.json
}

