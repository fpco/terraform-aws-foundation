# AWS Managed NAT Gateways
resource "aws_eip" "nat" {
  count = "${var.nat_count}"
  vpc   = true
}

resource "aws_nat_gateway" "nat" {
  count         = "${var.nat_count}"
  allocation_id = "${element(aws_eip.nat.*.id, count.index)}"
  subnet_id     = "${var.subnet_ids[count.index]}"
}
