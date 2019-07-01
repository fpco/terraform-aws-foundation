output "key_file" {
  value       = var.key_file
  description = "Export the `key_file` variable for convenience"
}

output "asg_name" {
  value       = module.prometheus-server.name
  description = "`name` exported from the Prometheus Server `aws_autoscaling_group`"
}

