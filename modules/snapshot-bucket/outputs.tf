output "arn" {
  value       = aws_s3_bucket.s3.arn
  description = "`arn` exported from `aws_s3_bucket`"
}

output "id" {
  value       = aws_s3_bucket.s3.id
  description = "`id` exported from `aws_s3_bucket`"
}

output "region" {
  value       = aws_s3_bucket.s3.region
  description = "`region` exported from `aws_s3_bucket`"
}

output "hosted_zone_id" {
  value       = aws_s3_bucket.s3.hosted_zone_id
  description = "`hosted_zone` exported from `aws_s3_bucket`"
}

output "aws_access_key_id" {
  value       = aws_iam_access_key.s3.id
  description = "`id` exported from `aws_iam_access_key`"
}

output "aws_secret_access_key" {
  value       = aws_iam_access_key.s3.secret
  description = "`secret` exported from `aws_iam_access_key`"
}

