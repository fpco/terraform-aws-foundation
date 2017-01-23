/**
 *## Virtual Private Cloud (VPC)
 *
 * This module takes care of VPC deployment. Scenarios 1 and 2 are possible with
 * this module: http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Scenarios.html
 * 
 */
resource "aws_vpc" "main" {
  cidr_block           = "${var.cidr}"
  enable_dns_hostnames = true
  enable_dns_support   = true
  tags = "${merge(map("Name", "${var.name_prefix}-vpc"), "${var.extra_tags}")}"
}

resource "aws_internet_gateway" "main" {
  vpc_id = "${aws_vpc.main.id}"

  tags = "${merge(map("Name", "${var.name_prefix}-igw"), "${var.extra_tags}")}"
}

resource "aws_vpc_dhcp_options" "main" {
  domain_name         = "${var.region}.compute.internal"
  domain_name_servers = ["AmazonProvidedDNS"]

  tags = "${merge(map("Name", "${var.name_prefix}-dhcp-options"), "${var.extra_tags}")}"
}

resource "aws_vpc_dhcp_options_association" "main" {
  vpc_id          = "${aws_vpc.main.id}"
  dhcp_options_id = "${aws_vpc_dhcp_options.main.id}"
}

resource "aws_subnet" "public" {
  count             = "${length(var.public_subnet_cidrs)}"
  vpc_id            = "${aws_vpc.main.id}"
  cidr_block        = "${var.public_subnet_cidrs[count.index]}"
  availability_zone = "${var.azs[count.index]}"
  tags              = "${merge(map("Name", "${var.name_prefix}-public-${var.azs[count.index]}"), "${var.extra_tags}")}"
}

resource "aws_subnet" "private" {
  count                   = "${length(var.private_subnet_cidrs)}"
  vpc_id                  = "${aws_vpc.main.id}"
  cidr_block              = "${var.private_subnet_cidrs[count.index]}"
  availability_zone       = "${var.azs[count.index]}"
  map_public_ip_on_launch = false
  tags                    = "${merge(map("Name", "${var.name_prefix}-private-${var.azs[count.index]}"), "${var.extra_tags}")}"
}

resource "aws_route_table" "public" {
  vpc_id = "${aws_vpc.main.id}"

  tags = "${merge(map("Name", "${var.name_prefix}-public"), "${var.extra_tags}")}"
}

resource "aws_route" "public_internet_gateway" {
  route_table_id         = "${aws_route_table.public.id}"
  gateway_id             = "${aws_internet_gateway.main.id}"
  destination_cidr_block = "0.0.0.0/0"
}

resource "aws_route_table_association" "public-rta" {
  count          = "${length(var.public_subnet_cidrs)}"
  subnet_id      = "${element(aws_subnet.public.*.id, count.index)}"
  route_table_id = "${aws_route_table.public.id}"
}


## NAT Gateways - provide internet access to instances in private subnets.

module "nat-gateways" {
  source     = "../nat-gateways"
  nat_count  = "${var.nat_count}"
  subnet_ids = ["${aws_subnet.public.*.id}"]
}  

# Route tables. One per private subnet.
resource "aws_route_table" "private" {
  count  = "${var.nat_count}"
  vpc_id = "${aws_vpc.main.id}"

  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = "${element(module.nat-gateways.ids, count.index)}"
  }

  tags = "${merge(map("Name", "${var.name_prefix}-private-${element(var.azs, count.index)}"), "${var.extra_tags}")}"
}

resource "aws_route_table_association" "private-rta" {
  count          = "${var.nat_count}"
  subnet_id      = "${element(aws_subnet.private.*.id, count.index)}"
  route_table_id = "${element(aws_route_table.private.*.id, count.index)}"
}

