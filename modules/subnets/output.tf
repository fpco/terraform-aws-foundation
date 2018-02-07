//List of subnet ids
output "ids" {
  value = ["${aws_subnet.main.*.id}"]
}

// CIDR blocks
output "cidr_blocks" {
  value = ["${aws_subnet.main.*.cidr_block}"]
}

// list of Availability Zones
output "azs" {
  value = ["${aws_subnet.main.*.availability_zone}"]
}

// ID of the VPC the subnets are in
output "vpc_id" {
  value = ["${var.vpc_id}"]
}
