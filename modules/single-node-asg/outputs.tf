output "asg_name" {
  value       = module.server.name
  description = "`name` exported from the Server `aws_autoscaling_group`"
}

output "asg_iam_profile_arn" {
  value       = module.service-data.iam_profile_arn
  description = "`arn` exported from the Service Data `aws_iam_profile`"
}

output "asg_iam_role_arn" {
  value       = module.service-data.iam_role_arn
  description = "`arn` exported from the Service Data `aws_iam_role`"
}

output "asg_iam_role_name" {
  value       = module.service-data.iam_role_name
  description = "`name` exported from the Service Data `aws_iam_role`"
}

