output "bastion_ip" {
  value       = aws_instance.bastion.public_ip
  description = "IP address of the bastion jump box"
}

output "region" {
  value       = var.region
  description = "AWS region of deployment"
}

output "vpc_id" {
  value       = module.vpc.vpc_id
  description = "Id of the created VPC"
}

output "public_subnet_ids" {
  value       = module.vpc.public_subnet_ids
  description = "Ids of created public subnets"
}

output "private_subnet_ids" {
  value       = module.vpc.private_subnet_ids
  description = "Ids of created private subnets"
}

output "ssh_key_name" {
  value       = aws_key_pair.main.key_name
  description = "SSH Key pair name"
}

