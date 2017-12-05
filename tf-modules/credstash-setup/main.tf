/**
 * ## Credstash Setup
 *
 * Setup KMS Master Key and a DynamoDB Table for use with Credstash.
 *
 * By default, this module doesn't need any variables to be set manually, but can
 * be overridden if necessary. By doing so it is possible to create either key or
 * database table or both, as well as other customizations.
 *
 * **Resources created here cannot be deleted using terraform and have to be deleted
 * manually. This behavior is to prevent possibility of sensitive data loss.**
 *
 */

resource "aws_kms_key" "credstash-key" {
  count               = "${var.create_kms_key ? 1 : 0}"
  description         = "Master key used by credstash"
  enable_key_rotation = "${var.enable_key_rotation}"

  tags = {
    Name = "${var.kms_key_name}"
  }
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

## Writer Policy

resource "aws_iam_policy" "writer-policy" {
  count = "${var.create_writer_policy ? 1 : 0}"
  name  = "${var.db_table_name}-writer"

  policy = <<END_POLICY
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "dynamodb:PutItem"
      ],
      "Effect": "Allow",
      "Resource": "arn:${var.aws_cloud}:dynamodb:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:table/${var.db_table_name}"
    }
  ]
}
END_POLICY
}

## Reader Policy

resource "aws_iam_policy" "reader-policy" {
  count = "${var.create_reader_policy ? 1 : 0}"
  name  = "${var.db_table_name}-reader"

  policy = <<END_POLICY
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "dynamodb:GetItem",
        "dynamodb:Query",
        "dynamodb:Scan"
      ],
      "Effect": "Allow",
      "Resource": "arn:${var.aws_cloud}:dynamodb:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:table/${var.db_table_name}"
    }
  ]
}
END_POLICY
}
