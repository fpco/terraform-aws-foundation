



output "bucket_name" {
  value = "${aws_s3_bucket.bucket.id}"
}

output "access_key" {
  value = "${aws_iam_access_key.cache-s3-user-access-key.id}"
}

output "secret_key" {
  value = "${aws_iam_access_key.cache-s3-user-access-key.encrypted_secret}"
}
