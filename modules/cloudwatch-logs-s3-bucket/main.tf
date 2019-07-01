data "aws_iam_policy_document" "cloudwatch-logs-bucket" {
  statement {
    effect  = "Allow"
    actions = ["s3:PutObject"]

    resources = ["arn:${var.aws_region}:s3:::${var.name_prefix}-cloudwatch-logs/*}"]

    principals {
      type        = "AWS"
      identifiers = var.principals
    }
  }
}

resource "aws_s3_bucket" "cloudwatch-logs" {
  bucket = "${var.name_prefix}-cloudwatch-logs"
  acl    = "private"
  policy = length(var.principals) == 0 ? "" : data.aws_iam_policy_document.cloudwatch-logs-writer.json

  tags = merge(
    {
      "Name" = "${var.name_prefix}-cloudwatch-logs"
    },
    var.extra_tags,
  )
}

