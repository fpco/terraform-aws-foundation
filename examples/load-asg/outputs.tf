##################
## Outputs

output "region" {
  value       = var.region
  description = "region deployed to"
}

output "server_name" {
  value       = module.web-asg.name
  description = "name of the autoscaling servers"
}

output "elb_fqdn" {
  value       = aws_elb.web.dns_name
  description = "FQDN DNS of the autoscaling ELB"
}

