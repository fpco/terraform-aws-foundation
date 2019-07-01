output "elb_dns" {
  value       = aws_elb.web.dns_name
  description = "make the ELB accessible on the outside"
}

output "vpc_cidr" {
  value       = var.vpc_cidr
  description = "VPC CIDR for ipsec.conf template"
}

# AWS VPN
output "vpn_tun1_ip" {
  value       = module.vpc.vpn_tun1_ip
  description = "IP of Tunnel 1"
  sensitive   = true
}

output "vpn_tun1_key" {
  value       = module.vpc.vpn_tun1_key
  description = "Pre-sharedkey of Tunnel 1"
  sensitive   = true
}

output "vpn_tun2_ip" {
  value       = module.vpc.vpn_tun2_ip
  description = "IP of Tunnel 2"
  sensitive   = true
}

output "vpn_tun2_key" {
  value       = module.vpc.vpn_tun2_key
  description = "Pre-sharedkey of Tunnel 2"
  sensitive   = true
}

