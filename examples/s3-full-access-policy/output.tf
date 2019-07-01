output "access_key_full_access_user" {
  sensitive = true
  value     = aws_iam_access_key.full-access-user-access-key.id
}

output "secret_key_full_access_user" {
  sensitive = true
  value     = aws_iam_access_key.full-access-user-access-key.secret
}

output "access_key_no_access_user" {
  sensitive = true
  value     = aws_iam_access_key.no-access-user-access-key.id
}

output "secret_key_no_access_user" {
  sensitive = true
  value     = aws_iam_access_key.no-access-user-access-key.secret
}

output "bucket_list" {
  description = "The list of bucket names in the bucket names variable list. This should eventually be a list of the buckets that were actually created."
  value       = var.bucket_names
}

