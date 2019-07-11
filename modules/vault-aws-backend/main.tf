provider "aws" {
  region = "${var.region}"
}

provider "vault" {
  address = "${var.vault_address}"
  token   = "${var.vault_token}"
}

resource "vault_aws_secret_backend" "aws" {
  region                    = "${var.region}"
  access_key                = "${var.access_key}"
  secret_key                = "${var.secret_key}"
  path                      = "${var.secret_backend_path}"
  default_lease_ttl_seconds = "${var.default_lease_ttl_seconds}"
  max_lease_ttl_seconds     = "${var.max_lease_ttl_seconds}"
}

resource "vault_aws_secret_backend_role" "aws_role" {
  backend         = "${vault_aws_secret_backend.aws.path}"
  name            = "${var.role_name}"
  credential_type = "${var.credential_type}"
  role_arns       = ["${var.role_arn}"]
}

