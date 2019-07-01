output "vpc_id" {
  value       = module.vpc.vpc_id
  description = "VPC id"
}

output "private_subnet_ids" {
  value       = module.private-subnets.ids
  description = "List of IDs of private subnets. None created if list is empty."
}

output "vpn_route_table_id" {
  value       = aws_route_table.private-vpn.id
  description = "Route table id associated with VPN and private subnets"
}

output "vgw_id" {
  value       = module.vpn.vpn_gw_id
  description = "VPN gateway id"
}

