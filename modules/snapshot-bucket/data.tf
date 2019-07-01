data "aws_partition" "current" {
}

data "aws_iam_policy_document" "s3" {
  statement {
    effect    = "Allow"
    actions   = ["s3:ListBucket"]
    resources = ["arn:aws:s3:::${var.name}"]
  }

  statement {
    effect = "Allow"

    actions = [
      "s3:ListObjects",
      "s3:PutObject",
      "s3:GetObject",
      "s3:CreateMultipartUpload",
    ]

    resources = ["arn:${data.aws_partition.current.partition}:s3:::${var.name}/*"]
  }
}

