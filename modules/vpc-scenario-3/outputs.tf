output "vpc_id" {
  value       = module.vpc.vpc_id
  description = "VPC id"
}

output "private_subnet_ids" {
  value       = module.private-subnets.ids
  description = "List of private subnet ids. None created if list is empty."
}

output "public_cidr_blocks" {
  value       = module.public-subnets.cidr_blocks
  description = "List of private subnet CIDR blocks"
}

output "public_subnet_ids" {
  value       = module.public-subnets.ids
  description = "List of public subnet ids"
}

output "public_route_table_id" {
  value       = module.public-gateway.route_table_id
  description = "Route table id associated with public subnets"
}

output "igw_id" {
  value       = module.public-gateway.gateway_id
  description = "Internet gateway id"
}

# AWS VPN

output "vpn_config" {
  value       = module.vpn.vpn_config
  description = "XML-formatted config for the VPN/Customer Gateway with DC2"
  sensitive   = true
}

output "vpn_tun1_ip" {
  value       = module.vpn.vpn_connection_tunnel1_address
  description = "IP of Tunnel 1"
  sensitive   = true
}

output "vpn_tun1_key" {
  value       = module.vpn.vpn_connection_tunnel1_preshared_key
  description = "Pre-sharedkey of Tunnel 1"
  sensitive   = true
}

output "vpn_tun2_ip" {
  value       = module.vpn.vpn_connection_tunnel2_address
  description = "IP of Tunnel 2"
  sensitive   = true
}

output "vpn_tun2_key" {
  value       = module.vpn.vpn_connection_tunnel2_preshared_key
  description = "Pre-sharedkey of Tunnel 2"
  sensitive   = true
}

