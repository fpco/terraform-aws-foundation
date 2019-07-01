output "name" {
  value       = aws_autoscaling_group.cluster.name
  description = "Name of the ASG"
}

output "id" {
  value       = aws_autoscaling_group.cluster.id
  description = "ID of the ASG"
}

output "lc_id" {
  value       = aws_launch_configuration.cluster.id
  description = "ID of the Launch Config"
}

output "lc_name" {
  value       = aws_launch_configuration.cluster.name
  description = "Name of the Launch Config"
}

