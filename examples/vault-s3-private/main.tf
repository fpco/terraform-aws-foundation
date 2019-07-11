resource "aws_s3_bucket" "vault-test-bucket" {
  bucket = "vault-fpco-test-bucket"
  acl    = "private"
  region = "us-east-2"

  tags = {
    Name        = "Vault test bucket"
    Environment = "Dev"
  }
}

# Here we allow everyone to assume this role. In production systems
# it's best to restrict it's scope so that only some IAM users are
# able to assume this role.
resource "aws_iam_role" "vault_bucket_role" {
  name = "bucket_access_role"

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
    Environment = "Dev"
  }
}

resource "aws_iam_role_policy" "vault_bucket_policy" {
  name = "bucket-policy"
  role = "${aws_iam_role.vault_bucket_role.id}"
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
  vault_address             = "${var.vault_address}"
  vault_token               = "${var.vault_token}"
  secret_backend_path       = "${var.secret_backend_path}"
  default_lease_ttl_seconds = "${var.default_lease_ttl_seconds}"
  max_lease_ttl_seconds     = "${var.max_lease_ttl_seconds}"
  credential_type           = "${var.credential_type}"
  role_name                 = "${var.role_name}"
  role_arn                  = "${aws_iam_role.vault_bucket_role.arn}"
  access_key                = "${var.access_key}"
  secret_key                = "${var.secret_key}"
}
