output "vpn_gw_id" {
  value       = aws_vpn_gateway.main.id
  description = "ID of the VPN Gateway (AWS-side)"
}

output "customer_gw_id" {
  value       = aws_customer_gateway.main.id
  description = "ID of the AWS Customer Gateway (client-side)"
}

output "aws_vpn_gateway_id" {
  value       = aws_vpn_gateway.main.id
  description = "ID of the AWS Virtualt Private Gateway"
}

output "customer_gw_ip" {
  value       = aws_customer_gateway.main.ip_address
  description = "IP address of the AWS Customer Gateway (client-side)"
}

output "vpn_connection_id" {
  value       = aws_vpn_connection.main.id
  description = "ID of the VPN Connection (AWS-side)"
}

output "vpn_connection_tunnel1_address" {
  value       = aws_vpn_connection.main.tunnel1_address
  description = "IP Address of the first VPN Tunnel in the Connection"
}

output "vpn_connection_tunnel1_preshared_key" {
  sensitive   = true
  value       = aws_vpn_connection.main.tunnel1_preshared_key
  description = "Preshared key of the first VPN Tunnel in the Connection"
}

output "vpn_connection_tunnel2_address" {
  value       = aws_vpn_connection.main.tunnel2_address
  description = "IP Address of the second VPN Tunnel in the Connection"
}

output "vpn_connection_tunnel2_preshared_key" {
  sensitive   = true
  value       = aws_vpn_connection.main.tunnel2_preshared_key
  description = "Preshared key of the second VPN Tunnel in the Connection"
}

output "vpn_config" {
  sensitive   = true
  value       = aws_vpn_connection.main.customer_gateway_configuration
  description = "XML-formatted config with sensitive info for setting up the client device"
}

output "vpn_connection_static_routes" {
  value       = aws_vpn_connection_route.main.*.destination_cidr_block
  description = "List of destination CIDR blocks, one for each (static) VPN Connection Route"
}

