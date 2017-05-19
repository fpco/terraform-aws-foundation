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
  lifecycle {
    prevent_destroy = true
  }
}

resource "aws_kms_alias" "credstash-key" {
  count         = "${var.create_kms_key ? 1 : 0}"
  name          = "alias/${var.kms_key_name}"
  target_key_id = "${aws_kms_key.credstash-key.key_id}"
  lifecycle {
    prevent_destroy = true
  }
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
  lifecycle {
    prevent_destroy = true
  }
}


//KMS Key Alias. It can later be used to store secrets:
// credstash put -k kms_key_alias secret_key secret_value
output "kms_key_alias" {
  value = "alias/${var.kms_key_name}"
}

//KMS Master key ARN.
output "kms_key_arn" {
  value = "${aws_kms_alias.credstash-key.arn}"
}

//KMS Master key id which can be used by credstash to store/retrieve secrets.
output "kms_key_arn" {
  value = "${aws_kms_alias.credstash-key.key_id}"
}

//DynamoDB table ARN that can be used by credstash to store/retrieve secrets.
output "db_table_arn" {
  value = "${aws_dynamodb_table.credstash-db.arn}"
}
