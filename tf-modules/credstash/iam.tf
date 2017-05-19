data "aws_caller_identity" "current" {}

data "aws_region" "current" {
  current = true
}


## Writer Policy

resource "aws_iam_policy" "writer-policy" {
  count = "${var.create_writer_policy ? 1 : 0}"
  name = "${var.name_prefix}-credstash-writer"
  policy = <<END_POLICY
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "kms:GenerateDataKey"
      ],
      "Effect": "Allow",
      "Resource": "${var.kms_key_arn}"
    },
    {
      "Action": [
        "dynamodb:PutItem"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:dynamodb:${coalesce(var.region, data.aws_region.current.name)}:${coalesce(var.account_id, data.aws_caller_identity.current.account_id)}:table/${var.table_name}"
    }
  ]
}
END_POLICY
}


## Reader Policy

resource "aws_iam_policy" "reader-policy" {
  count = "${var.create_reader_policy ? 1 : 0}"
  name = "${var.name_prefix}-credstash-reader"
  policy = <<END_POLICY
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "kms:Decrypt"
      ],
      "Effect": "Allow",
      "Resource": "${var.kms_key_arn}"
    },
    {
      "Action": [
        "dynamodb:GetItem",
        "dynamodb:Query",
        "dynamodb:Scan"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:dynamodb:${coalesce(var.region, data.aws_region.current.name)}:${coalesce(var.account_id, data.aws_caller_identity.current.account_id)}:table/${var.table_name}"
    }
  ]
}
END_POLICY
}
