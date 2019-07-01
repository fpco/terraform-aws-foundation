#----------------------------------------------------------------------
# Roles
#----------------------------------------------------------------------

module "admin-role" {
  source = "../cross-account-role"
  name   = "admin"

  trust_account_ids = concat([data.aws_caller_identity.current.account_id],
    var.trust_account_ids)
}

module "power-user-role" {
  source = "../cross-account-role"
  name   = "power-user"

  trust_account_ids = concat([data.aws_caller_identity.current.account_id],
    var.trust_account_ids)
}

