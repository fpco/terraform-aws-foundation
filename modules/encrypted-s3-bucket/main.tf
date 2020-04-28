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
