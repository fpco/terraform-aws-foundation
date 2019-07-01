output "id" {
  value       = aws_vpc.core.id
  description = "`id` exported from `aws_vpc`"
}

output "cidr_block" {
  value       = aws_vpc.core.cidr_block
  description = "`cidr_block` exported from `aws_vpc`"
}

output "route_table_id" {
  value       = aws_route_table.core.id
  description = "`id` exported from `aws_route_table`"
}

output "igw_id" {
  value       = aws_internet_gateway.core.id
  description = "`id` exported from `aws_internet_gateway`"
}

