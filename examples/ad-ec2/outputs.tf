output "microsoft-ad_dns_ip_addresses" {
  description = "Microsoft AD DNS IP Address"
  value       = aws_directory_service_directory.main.dns_ip_addresses
}

output "microsoft-ad_dns_name" {
  description = "Microsoft AD DNS Name"
  value       = aws_directory_service_directory.main.name
}

output "windows_ec2_public_dns" {
  description = "Widnows EC2 Public DNS"
  value       = aws_instance.web.public_dns
}
