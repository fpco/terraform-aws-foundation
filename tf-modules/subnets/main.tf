/**
 *## Subnets creation
 *
 * This module can create public and private subnets while interleaving them
 * throughout supplied availiability zones.
 *
 */


resource "aws_subnet" "public" {
  count                   = "${length(var.public_subnet_cidrs)}"
  vpc_id                  = "${var.vpc_id}"
  cidr_block              = "${var.public_subnet_cidrs[count.index]}"
  availability_zone       = "${element(var.azs, count.index)}"
  map_public_ip_on_launch = true
  tags                    = "${merge(map("Name", "${var.name_prefix}-public-${format("%02d", count.index)}-${element(var.azs, count.index)}"), "${var.extra_tags}")}"
}

resource "aws_subnet" "private" {
  count                   = "${length(var.private_subnet_cidrs)}"
  vpc_id                  = "${var.vpc_id}"
  cidr_block              = "${var.private_subnet_cidrs[count.index]}"
  availability_zone       = "${element(var.azs, count.index)}"
  map_public_ip_on_launch = false
  tags                    = "${merge(map("Name", "${var.name_prefix}-private-${format("%02d", count.index)}-${element(var.azs, count.index)}"), "${var.extra_tags}")}"
}

