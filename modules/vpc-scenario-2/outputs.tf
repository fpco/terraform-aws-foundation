output "vpc_id" {
  value       = module.vpc.vpc_id
  description = "VPC id"
}

output "vpc_cidr_block" {
  value       = module.vpc.vpc_cidr_block
  description = "VPC CIDR block"
}

output "public_subnet_ids" {
  value       = module.public-subnets.ids
  description = "List of public subnet ids"
}

output "public_cidr_blocks" {
  value       = module.public-subnets.cidr_blocks
  description = "List of private subnet CIDR blocks"
}

output "public_route_table_id" {
  value       = module.public-gateway.route_table_id
  description = "Route table id associated with public subnets"
}

output "igw_id" {
  value       = module.public-gateway.gateway_id
  description = "Internet gateway id"
}

output "private_subnet_ids" {
  value       = module.private-subnets.ids
  description = "List of private subnet ids"
}

output "private_cidr_blocks" {
  value       = module.private-subnets.cidr_blocks
  description = "List of private subnet CIDR blocks"
}

