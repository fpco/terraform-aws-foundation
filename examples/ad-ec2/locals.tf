locals {
  stage       = "dev"
  base_domain = "fpcomplete.local"
  domain      = "${local.stage}.${local.base_domain}"
}
