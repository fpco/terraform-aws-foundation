output "region" {
  value       = var.region
  description = "region we are deploying the workspace to"
}

output "public_dns" {
  value       = aws_route53_record.workspace.name
  description = "CNAME DNS record pointing at the EC2 instance"
}

output "instance_id" {
  value       = aws_instance.workspace.id
  description = "ID of the EC2 instance"
}

