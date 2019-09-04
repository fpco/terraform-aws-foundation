output "volume_id" {
  value       = aws_ebs_volume.main.id
  description = "`id` exported from the `aws_ebs_volume`"
}

output "iam_policy_arn" {
  value       = aws_ebs_volume.main.id
  description = "`id` exported from the `aws_ebs_volume`"
}

output "iam_profile_policy_document" {
  value       = aws_iam_policy.attach_ebs.policy
  description = "`policy` exported from the `aws_iam_policy`"
}
