//VPC id
output "vpc_id" {
  value = "${module.vpc.vpc_id}"
}

//List of private subnet ids. None created if list is empty.
output "private_subnet_ids" {
  value = ["${module.subnets.private_ids}"]
}

//List of public subnet ids
output "public_subnet_ids" {
  value = ["${module.subnets.public_ids}"]
}

// Route table id associated with public subnets
output "public_route_table_id" {
  value = "${aws_route_table.public.id}"
}

//Internet gateway id
output "igw_id" {
  value = ["${module.private-subnets.ids}"]
}
