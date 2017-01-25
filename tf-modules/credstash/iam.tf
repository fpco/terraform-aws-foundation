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
      "Resource": "arn:aws:kms:${var.region == "" ? data.aws_region.current.name : var.region}:${var.account_id == "" ? data.aws_caller_identity.current.account_id : var.account_id}:key/${kms_key_id}"
    },
    {
      "Action": [
        "dynamodb:PutItem"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:dynamodb:${var.region == "" ? data.aws_region.current.name : var.region}:${var.account_id == "" ? data.aws_caller_identity.current.account_id : var.account_id}:table/credential-store"
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
      "Resource": "arn:aws:kms:${var.region == "" ? data.aws_region.current.name : var.region}:${var.account_id == "" ? data.aws_caller_identity.current.account_id : var.account_id}:key/${kms_key_id}"
    },
    {
      "Action": [
        "dynamodb:GetItem",
        "dynamodb:Query",
        "dynamodb:Scan"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:dynamodb:${var.region == "" ? data.aws_region.current.name : var.region}:${var.account_id == "" ? data.aws_caller_identity.current.account_id : var.account_id}:table/credential-store"
    }
  ]
}
END_POLICY
}
