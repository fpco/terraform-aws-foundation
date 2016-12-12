/**
 *## AWS Managed NAT Gateway
 *
 *Wraps `aws_eip` and `aws_nat_gateway`.
 */
resource "aws_eip" "nat" {
    vpc = true
}
resource "aws_nat_gateway" "nat" {
    allocation_id = "${aws_eip.nat.id}"
    subnet_id = "${var.subnet_id}"
}
