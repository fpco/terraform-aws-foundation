output "asg_name" {
  value       = module.server.name
  description = "`name` exported from the Server `aws_autoscaling_group`"
}
