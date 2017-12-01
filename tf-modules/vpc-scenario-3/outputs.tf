//VPC id
output "vpc_id" {
  value = "${module.vpc.vpc_id}"
}

//List of private subnet ids. None created if list is empty.
output "private_subnet_ids" {
  value = ["${module.private-subnets.ids}"]
}

//List of public subnet ids
output "public_subnet_ids" {
  value = ["${module.public-subnets.ids}"]
}

// Route table id associated with public subnets
output "public_route_table_id" {
  value = "${module.public-gateway.route_table_id}"
}

//Internet gateway id
output "igw_id" {
  value = "${module.public-gateway.gateway_id}"
}

# AWS VPN

// XML-formatted config for the VPN/Customer Gateway with DC2
output "vpn_config" {
  value     = "${module.vpn.vpn_config}"
  sensitive = true
}

// IP of Tunnel 1
output "vpn_tun1_ip" {
  value     = "${module.vpn.vpn_connection_tunnel1_address}"
  sensitive = true
}

// Pre-sharedkey of Tunnel 1
output "vpn_tun1_key" {
  value     = "${module.vpn.vpn_connection_tunnel1_preshared_key}"
  sensitive = true
}

// IP of Tunnel 2
output "vpn_tun2_ip" {
  value     = "${module.vpn.vpn_connection_tunnel2_address}"
  sensitive = true
}

// Pre-sharedkey of Tunnel 2
output "vpn_tun2_key" {
  value     = "${module.vpn.vpn_connection_tunnel2_preshared_key}"
  sensitive = true
}
