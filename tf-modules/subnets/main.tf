/**
 *## Subnets creation
 *
 * This module creates one or more subnets, interleaving them across a list of
 * availiability zones.
 *
 */

resource "aws_subnet" "main" {
  count             = "${length(var.cidr_blocks)}"
  vpc_id            = "${var.vpc_id}"
  cidr_block        = "${var.cidr_blocks[count.index]}"
  availability_zone = "${element(var.azs, count.index)}"
  tags              = "${merge(map("Name", "${var.name_prefix}-${format("%02d", count.index)}-${element(var.azs, count.index)}"), "${var.extra_tags}")}"
  map_public_ip_on_launch = "${var.public}"
}
