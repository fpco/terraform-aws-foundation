output "vpc_id" {
  value       = "${aws_vpc.main.id}"
  description = "VPC id"
}

output "private_subnet_ids" {
  value       = ["${module.subnets.private_ids}"]
  description = "List of private subnet ids. None created if list is empty."
}

output "public_subnet_ids" {
  value       = ["${module.subnets.public_ids}"]
  description = "List of public subnet ids"
}

output "public_route_table_id" {
  value       = "${aws_route_table.public.id}"
  description = "Route table id associated with public subnets"
}

output "igw_id" {
  value       = "${aws_internet_gateway.main.id}"
  description = "Internet gateway id"
}
