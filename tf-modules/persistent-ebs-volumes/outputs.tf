//IAM policy ARN, which can further be used to attach to a role policy
output "iam_volume_policy_arns" {
  value = ["${aws_iam_policy.ebs-volume-policy.*.arn}"]
}
//IDs of EBS volumes
output "volume_ids" {
  value = ["${aws_ebs_volume.volumes.*.id}"]
}
