output "vpc_id" {
  value       = aws_vpc.main.id
  description = "ID from `aws_vpc`"
}

output "public_subnets" {
  value       = aws_subnet.public.*.id
  description = "List of IDs of public subnets"
}

output "public_route_table_id" {
  value       = aws_route_table.public.id
  description = "List of IDs of routing tables"
}

output "igw_id" {
  value       = aws_internet_gateway.main.id
  description = "ID of internet gateway"
}

