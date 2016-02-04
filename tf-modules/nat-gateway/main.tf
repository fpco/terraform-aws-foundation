# AWS Managed NAT Gateway
resource "aws_network_interface" "nat" {
    subnet_id = "${var.subnet_id}"
}
resource "aws_eip" "nat" {
    network_interface = "${aws_network_interface.nat.id}"
    vpc = true
}
resource "aws_nat_gateway" "nat" {
    allocation_id = "${aws_eip.nat.id}"
    subnet_id = "${var.subnet_id}"
}
