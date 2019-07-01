output "gateway_id" {
  value = module.public-gateway.gateway_id
}

output "route_table_id" {
  value = module.public-gateway.route_table_id
}

output "subnet_ids" {
  value = module.public-subnets.ids
}

output "subnet_cidrs" {
  value = module.public-subnets.cidr_blocks
}

output "subnet_azs" {
  value = module.public-subnets.azs
}

output "vpc_id" {
  value = module.vpc.vpc_id
}

output "vpc_cidr" {
  value = module.vpc.vpc_cidr_block
}

output "vpc_dhcp_options_id" {
  value = module.vpc.dhcp_options_id
}

output "kops_state_bucket" {
  value = module.kops-state-bucket.bucket_id
}

output "region" {
  value = var.region
}

output "azs" {
  value = var.availability_zones
}

output "name_prefix" {
  value = var.name_prefix
}

output "kubernetes_cluster_name" {
  value = var.kubernetes_cluster_name
}

