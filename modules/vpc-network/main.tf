/**
 * ## VPC with Basic Network
 *
 * This module will create:
 *
 * * `aws_vpc`
 * * `aws_internet_gateway`
 * * `aws_route_table`
 * * `aws_main_route_table_association`
 *
 * We recommend using the `vpc-scenario-*` modules instead of this one.
 *
 * **DEPRECATED: MIGHT BE REMOVED IN A FUTURE RELEASE**
 *
 */
resource "aws_vpc" "core" {
  cidr_block           = "${var.vpc_cidr_prefix}.0.0/16"
  enable_dns_hostnames = "${var.enable_dns_hostnames}"

  tags {
    Name = "${var.name}"
  }
}

resource "aws_internet_gateway" "core" {
  vpc_id = "${aws_vpc.core.id}"

  tags {
    Name = "${var.name}"
  }
}

resource "aws_route_table" "core" {
  vpc_id = "${aws_vpc.core.id}"

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = "${aws_internet_gateway.core.id}"
  }

  tags {
    Name = "${var.name}"
  }
}

# For more details on this next bit, see the notes section here:
# http://www.terraform.io/docs/providers/aws/r/main_route_table_assoc.html
resource "aws_main_route_table_association" "core" {
  vpc_id         = "${aws_vpc.core.id}"
  route_table_id = "${aws_route_table.core.id}"
}
