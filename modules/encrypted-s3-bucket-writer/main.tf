data "aws_s3_bucket" "enc" {
  bucket = var.bucket_name
}

data "aws_iam_policy_document" "writer" {
  statement {
    actions   = ["kms:Decrypt", "kms:GenerateDataKey"]
    resources = [var.key_arn]
  }
}

resource "aws_iam_policy" "writer" {
  policy = data.aws_iam_policy_document.writer.json
}

resource "aws_iam_role_policy_attachment" "writer" {
  role       = var.writer_role_name
  policy_arn = aws_iam_policy.writer.arn
}

data "aws_iam_policy_document" "s3_writer" {
  statement {
    actions   = ["s3:PutObject", "s3:DeleteObject"]
    resources = ["${data.aws_s3_bucket.enc.arn}/*"]
  }
}

resource "aws_iam_policy" "s3_writer" {
  policy = data.aws_iam_policy_document.s3_writer.json
}

resource "aws_iam_role_policy_attachment" "s3_writer" {
  role       = var.writer_role_name
  policy_arn = aws_iam_policy.s3_writer.arn
}
