output "az_a" {
  value       = aws_subnet.a.availability_zone
  description = "Availability Zone for subnet A"
}

output "id_a" {
  value       = aws_subnet.a.id
  description = "Subnet ID"
}

output "cidr_a" {
  value       = aws_subnet.a.cidr_block
  description = "Subnet A CIDR block"
}

output "az_c" {
  value       = aws_subnet.c.availability_zone
  description = "Availability Zone for subnet C"
}

output "id_c" {
  value       = aws_subnet.c.id
  description = "Subnet ID"
}

output "cidr_c" {
  value       = aws_subnet.c.cidr_block
  description = "Subnet C CIDR block"
}

