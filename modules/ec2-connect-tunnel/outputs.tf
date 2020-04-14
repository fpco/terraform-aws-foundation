output "public_ip" {
  value       = module.asg.eip_address
  description = "Public IP of the tunnel"
}
