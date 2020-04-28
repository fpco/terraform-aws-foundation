output "key_arn" {
  value = aws_kms_key.s3_enc.arn
}

output "bucket_name" {
  value = aws_s3_bucket.enc.id
}
