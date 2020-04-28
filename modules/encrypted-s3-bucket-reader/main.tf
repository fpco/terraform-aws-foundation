data "aws_s3_bucket" "enc" {
  bucket = var.bucket_name
}

data "aws_iam_policy_document" "reader" {
  statement {
    actions   = ["kms:Decrypt"]
    resources = [var.key_arn]
  }
}

resource "aws_iam_policy" "reader" {
  policy = data.aws_iam_policy_document.reader.json
}

resource "aws_iam_role_policy_attachment" "reader" {
  role       = var.reader_role_name
  policy_arn = aws_iam_policy.reader.arn
}

data "aws_iam_policy_document" "s3_reader" {
  statement {
    actions   = ["s3:Get*", "s3:List*"]
    resources = ["${data.aws_s3_bucket.enc.arn}/*"]
  }
}

resource "aws_iam_policy" "s3_reader" {
  policy = data.aws_iam_policy_document.s3_reader.json
}

resource "aws_iam_role_policy_attachment" "s3_reader" {
  role       = var.reader_role_name
  policy_arn = aws_iam_policy.s3_reader.arn
}
