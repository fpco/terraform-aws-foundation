output "subnet_a_id" {
  value       = module.cluster-net.id_a
  description = "ID for subnet A"
}

output "subnet_a_cidr" {
  value       = module.cluster-net.cidr_a
  description = "CIDR block for subnet A"
}

output "subnet_c_id" {
  value       = module.cluster-net.id_c
  description = "ID for subnet C"
}

output "subnet_c_cidr" {
  value       = module.cluster-net.cidr_c
  description = "CIDR block for subnet C"
}

output "asg_name" {
  value       = module.agent-asg.name
  description = "Name of the Auto-Scaling Group"
}

