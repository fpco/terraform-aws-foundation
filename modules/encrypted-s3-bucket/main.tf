resource "aws_kms_key" "s3_enc" {
  description             = "Key for S3 buckets"
  deletion_window_in_days = 10
}

resource "aws_s3_bucket" "enc" {
  acl    = "private"
  bucket = var.bucket_name
  server_side_encryption_configuration {
    rule {
      apply_server_side_encryption_by_default {
        kms_master_key_id = aws_kms_key.s3_enc.arn
        sse_algorithm     = "aws:kms"
      }
    }
  }
}

data "aws_iam_policy_document" "reader" {
  statement {
    actions   = ["kms:Decrypt"]
    resources = [aws_kms_key.s3_enc.arn]
  }
}

resource "aws_iam_policy" "reader" {
  policy = data.aws_iam_policy_document.reader.json
}

resource "aws_iam_role_policy_attachment" "reader" {
  count      = var.reader_role_name == "" ? 0 : 1
  role       = var.reader_role_name
  policy_arn = aws_iam_policy.reader.arn
}

data "aws_iam_policy_document" "writer" {
  statement {
    actions   = ["kms:Decrypt", "kms:GenerateDataKey"]
    resources = [aws_kms_key.s3_enc.arn]
  }
}

resource "aws_iam_policy" "writer" {
  policy = data.aws_iam_policy_document.writer.json
}

resource "aws_iam_role_policy_attachment" "writer" {
  count      = var.writer_role_name == "" ? 0 : 1
  role       = var.writer_role_name
  policy_arn = aws_iam_policy.writer.arn
}

data "aws_iam_policy_document" "s3_reader" {
  statement {
    actions   = ["s3:Get*", "s3:List*"]
    resources = ["${aws_s3_bucket.enc.arn}/*"]
  }
}

resource "aws_iam_policy" "s3_reader" {
  policy = data.aws_iam_policy_document.s3_reader.json
}

resource "aws_iam_role_policy_attachment" "s3_reader" {
  count      = var.reader_role_name == "" ? 0 : 1
  role       = var.reader_role_name
  policy_arn = aws_iam_policy.s3_reader.arn
}

data "aws_iam_policy_document" "s3_writer" {
  statement {
    actions   = ["s3:PutObject", "s3:DeleteObject"]
    resources = ["${aws_s3_bucket.enc.arn}/*"]
  }
}

resource "aws_iam_policy" "s3_writer" {
  policy = data.aws_iam_policy_document.s3_writer.json
}

resource "aws_iam_role_policy_attachment" "s3_writer" {
  count      = var.writer_role_name == "" ? 0 : 1
  role       = var.writer_role_name
  policy_arn = aws_iam_policy.s3_writer.arn
}

output "s3_bucket_name" {
  value       = aws_s3_bucket.enc.bucket
  description = "Name of the encrypted bucket"
}
