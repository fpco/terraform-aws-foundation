# id of S3 bucket
output "s3_bucket_id" {
  value = "${aws_s3_bucket.cloudwatch-logs.id}"
}

# ARN of S3 bucket
output "s3_bucket_arn" {
  value = "${aws_s3_bucket.cloudwatch-logs.arn}"
}
