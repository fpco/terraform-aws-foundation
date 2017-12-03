/**
 * ## Packer VPC
 *
 * Create a super simple, but isolated VPC on AWS, to be dedicated to building
 * AMIs with `packer`.
 *
 */

# THE VPC with an internet gateway
resource "aws_vpc" "packer" {
  cidr_block           = "${var.vpc_cidr_prefix}.0.0/16"
  enable_dns_hostnames = true

  tags {
    Name = "${var.name}"
  }
}

resource "aws_internet_gateway" "packer" {
  vpc_id = "${aws_vpc.packer.id}"

  tags {
    Name = "${var.name}"
  }
}

resource "aws_route_table" "packer" {
  vpc_id = "${aws_vpc.packer.id}"

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = "${aws_internet_gateway.packer.id}"
  }

  tags {
    Name = "${var.name}"
  }
}

resource "aws_subnet" "packer" {
  availability_zone       = "${element(data.aws_availability_zones.available.names, 0)}"
  vpc_id                  = "${aws_vpc.packer.id}"
  cidr_block              = "${var.vpc_cidr_prefix}.1.0/24"
  map_public_ip_on_launch = true

  tags {
    Name = "${var.name}"
  }
}

resource "aws_route_table_association" "packer" {
  subnet_id      = "${aws_subnet.packer.id}"
  route_table_id = "${aws_route_table.packer.id}"
}
