output "asg_name" {
  value       = module.server.name
  description = "`name` exported from the Server `aws_autoscaling_group`"
}

output "asg_iam_role_arn" {
  value       = module.instance_profile.iam_role_arn
  description = "The Role ARN of the instance profile associated with the instance in the ASG"
}

output "asg_iam_role_name" {
  value       = module.instance_profile.iam_role_name
  description = "`name` exported from the Service Data `aws_iam_role`"
}
