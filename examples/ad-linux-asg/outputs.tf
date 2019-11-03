output "microsoft-ad_dns_ip_addresses" {
  description = "Microsoft AD DNS IP Address"
  value       = aws_directory_service_directory.main.dns_ip_addresses
}

output "microsoft-ad_dns_name" {
  description = "Microsoft AD DNS Name"
  value       = aws_directory_service_directory.main.name
}

output "asg-name" {
  description = "ASG Name"
  value       = module.web-asg.name
}
