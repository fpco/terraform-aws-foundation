output "s3_bucket_id" {
  value       = aws_s3_bucket.cloudwatch-logs.id
  description = "id of S3 bucket"
}

output "s3_bucket_arn" {
  value       = aws_s3_bucket.cloudwatch-logs.arn
  description = "cloudwatch-logs-s3-bucket"
}

output "iam_instance_profile_name" {
  value       = aws_iam_instance_profile.cloudwatch-logs-writer.name
  description = "IAM instance profile for log writers"
}

output "iam_role_policy_json" {
  value       = data.aws_iam_policy_document.cloudwatch-logs-writer.json
  description = "Instances IAM role document for use with user-defined role policies."
}

