//VPC id
output "vpc_id" {
  value = "${module.vpc.vpc_id}"
}

//VPC CIDR block
output "vpc_cidr_block" {
  value = "${module.vpc.vpc_cidr_block}"
}

//List of public subnet ids
output "public_subnet_ids" {
  value = ["${module.public-subnets.ids}"]
}

//List of private subnet CIDR blocks
output "public_cidr_blocks" {
  value = ["${module.public-subnets.cidr_blocks}"]
}

// Route table id associated with public subnets
output "public_route_table_id" {
  value = "${module.public-gateway.route_table_id}"
}

//Internet gateway id
output "igw_id" {
  value = "${module.public-gateway.gateway_id}"
}

//List of private subnet ids
output "private_subnet_ids" {
  value = ["${module.private-subnets.ids}"]
}

//List of private subnet CIDR blocks
output "private_cidr_blocks" {
  value = ["${module.private-subnets.cidr_blocks}"]
}
