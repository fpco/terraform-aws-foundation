data "aws_region" "current" {}

locals {
  name_prefix = "${var.application}-${var.environment}"
  region      = data.aws_region.current.name
}

resource "aws_s3_bucket" "vault-test-bucket" {
  bucket = "${local.name_prefix}-bucket"
  acl    = "private"
  region = local.region

  tags = {
    Environment = var.environment
  }
}

# Here we allow everyone to assume this role. In production systems
# it's best to restrict it's scope so that only some IAM users are
# able to assume this role.
resource "aws_iam_role" "vault_bucket_role" {
  name = "${local.name_prefix}-bucket-role"

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "AWS": "*"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF


  tags = {
    Environment = var.environment
  }
}

resource "aws_iam_role_policy" "vault_bucket_policy" {
  name = "${local.name_prefix}-bucket-policy"
  role = aws_iam_role.vault_bucket_role.id
  policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": ["s3:*", "iam:CreateAccessKey"],
      "Resource": ["${aws_s3_bucket.vault-test-bucket.arn}"]
    }
  ]
}
EOF

}

module "vault_aws_backend" {
source                    = "../../modules/vault-aws-backend/"
vault_address             = var.vault_address
vault_token               = var.vault_token
secret_backend_path       = var.secret_backend_path
default_lease_ttl_seconds = var.default_lease_ttl_seconds
max_lease_ttl_seconds     = var.max_lease_ttl_seconds
credential_type           = var.credential_type
role_name                 = var.role_name
role_arn                  = aws_iam_role.vault_bucket_role.arn
access_key                = var.access_key
secret_key                = var.secret_key
}

