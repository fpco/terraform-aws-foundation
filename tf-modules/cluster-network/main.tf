/**
 * ## cluster-network
 * 
 * **DEPRECATED** - This module will be removed in a future release.
 * 
 * This module creates subnets in each of two Availability Zones in the region
 * specified. Specifically, an `aws_subnet` and `aws_route_table_association`
 * for each as a pair, named `a` and `c`.
 * 
 */

# Subnets in each of two AZ in this region
# (a and c are the only two in all three US regions)
# support 3 AZ / subnets in the future..
# also use the AWS AZ terraform module to lookup the 0,1,2 AZ based on Region
resource "aws_subnet" "a" {
  availability_zone       = "${var.region}a"
  cidr_block              = "${var.cidr_a}"
  map_public_ip_on_launch = "${var.public_ip}"
  vpc_id                  = "${var.vpc_id}"

  tags {
    Name = "${var.name}-${var.region}-a"
    Desc = "${var.description}"
  }
}

resource "aws_subnet" "c" {
  availability_zone       = "${var.region}c"
  cidr_block              = "${var.cidr_c}"
  map_public_ip_on_launch = "${var.public_ip}"
  vpc_id                  = "${var.vpc_id}"

  tags {
    Name = "${var.name}-${var.region}-c"
    Desc = "${var.description}"
  }
}

# Routing table association for each minion subnet to the VPC GW
resource "aws_route_table_association" "a" {
  route_table_id = "${var.route_table_id}"
  subnet_id      = "${aws_subnet.a.id}"
}

resource "aws_route_table_association" "c" {
  route_table_id = "${var.route_table_id}"
  subnet_id      = "${aws_subnet.c.id}"
}
