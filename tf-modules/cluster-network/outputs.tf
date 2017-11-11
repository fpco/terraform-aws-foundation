// Availability Zone for subnet A
output "az_a" {
  value = "${aws_subnet.a.availability_zone}"
}

// Subnet ID
output "id_a" {
  value = "${aws_subnet.a.id}"
}

// Subnet A CIDR block
output "cidr_a" {
  value = "${aws_subnet.a.cidr_block}"
}

// Availability Zone for subnet C
output "az_c" {
  value = "${aws_subnet.c.availability_zone}"
}

// Subnet ID
output "id_c" {
  value = "${aws_subnet.c.id}"
}

// Subnet A CIDR block
output "cidr_c" {
  value = "${aws_subnet.c.cidr_block}"
}
