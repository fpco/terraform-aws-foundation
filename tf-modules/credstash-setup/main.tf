/**
 * ## Setup KMS Master Key and a DynamoDB Table, which are required for credstash to work.
 *
 * By default, this module doesn't need any variables to be set manually, but can
 * be overridden if necessary. By doing so it is possible to create either key or
 * database table or both, as well as other customizations.
 * Resources created here cannot be deleted using terraform and have to be deleted
 * manually. This behavior is to prevent possibility of sensitive data loss.
 *
 */

resource "aws_kms_key" "credstash-key" {
  count               = "${var.create_kms_key ? 1 : 0}"
  description         = "Master key used by credstash"
  policy              = "${var.kms_key_policy}"
  enable_key_rotation = "${var.enable_key_rotation}"
}

resource "aws_kms_alias" "credstash-key" {
  count         = "${var.create_kms_key ? 1 : 0}"
  name          = "alias/${var.kms_key_name}"
  target_key_id = "${aws_kms_key.credstash-key.key_id}"
}


resource "aws_dynamodb_table" "credstash-db" {
  count          = "${var.create_db_table ? 1 : 0}"
  name           = "${var.db_table_name}"
  read_capacity  = 1
  write_capacity = 1
  hash_key       = "name"
  range_key      = "version"
  attribute {
    name = "name"
    type = "S"
  }
  attribute {
    name = "version"
    type = "S"
  }
}


data "aws_caller_identity" "current" {}

data "aws_region" "current" {
  current = true
}


//KMS Key ARN. It can later be used to store and retrieve secrets:
// credstash put -k kms_key_arn secret_key secret_value
// credstash get -k kms_key_arn secret_key
output "kms_key_arn" {
  value = "${aws_kms_key.credstash-key.arn}"
}

//KMS Master key id which can be used by credstash to store/retrieve secrets.
output "kms_key_id" {
  value = "${aws_kms_key.credstash-key.key_id}"
}

//KMS Master key ARN.
output "kms_key_alias" {
  value = "${aws_kms_alias.credstash-key.name}"
}

//KMS Master key ARN.
output "kms_key_alias_arn" {
  value = "${aws_kms_alias.credstash-key.arn}"
}

//DynamoDB table ARN that can be used by credstash to store/retrieve secrets.
output "db_table_arn" {
  value = "${aws_dynamodb_table.credstash-db.arn}"
}
