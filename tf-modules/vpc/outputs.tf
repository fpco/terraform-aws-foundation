// VPC ID
output "vpc_id" {
  value = "${aws_vpc.main.id}"
}

// VPC CIDR block
output "vpc_cidr_block" {
  value = "${aws_vpc.main.cidr_block}"
}

// ID of the DHCP options resource
output "dhcp_options_id" {
  value = "${aws_vpc_dhcp_options.main.id}"
}
