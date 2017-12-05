/**
 * ## Legacy VPC Module
 *
 * This module takes care of VPC (Virtual Private Cloud) deployment. Scenarios 1 and 2 are possible with
 * this module:
 * http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Scenarios.html
 *
 * We recommend using the new `vpc-scenario-*` modules instead of this one.
 *
 * **DEPRECATED: WILL BE REMOVED IN A FUTURE RELEASE**
 *
 */
resource "aws_vpc" "main" {
  cidr_block           = "${var.cidr}"
  enable_dns_hostnames = true
  enable_dns_support   = true
  tags                 = "${merge(map("Name", "${var.name_prefix}-vpc"), "${var.extra_tags}")}"
}

resource "aws_vpc_dhcp_options" "main" {
  domain_name         = "${var.region}.compute.internal"
  domain_name_servers = ["${var.dns_server_list}"]

  tags = "${merge(map("Name", "${var.name_prefix}-dhcp-options"), "${var.extra_tags}")}"
}

resource "aws_vpc_dhcp_options_association" "main" {
  vpc_id          = "${aws_vpc.main.id}"
  dhcp_options_id = "${aws_vpc_dhcp_options.main.id}"
}

module "subnets" {
  source               = "../subnets"
  azs                  = "${var.azs}"
  vpc_id               = "${aws_vpc.main.id}"
  name_prefix          = "${var.name_prefix}"
  public_subnet_cidrs  = "${var.public_subnet_cidrs}"
  private_subnet_cidrs = "${var.private_subnet_cidrs}"
  extra_tags           = "${var.extra_tags}"
}

## Internet Gateway - provide internet access to public subnets.

resource "aws_internet_gateway" "main" {
  vpc_id = "${aws_vpc.main.id}"

  tags = "${merge(map("Name", "${var.name_prefix}-igw"), "${var.extra_tags}")}"
}

resource "aws_route_table" "public" {
  vpc_id = "${aws_vpc.main.id}"

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = "${aws_internet_gateway.main.id}"
  }

  tags = "${merge(map("Name", "${var.name_prefix}-public"), "${var.extra_tags}")}"
}

resource "aws_route_table_association" "public-rta" {
  count          = "${length(var.public_subnet_cidrs)}"
  subnet_id      = "${element(module.subnets.public_ids, count.index)}"
  route_table_id = "${aws_route_table.public.id}"
}

## NAT Gateways - provide internet access to private subnets.

module "nat-gateways" {
  source             = "../nat-gateways"
  vpc_id             = "${aws_vpc.main.id}"
  name_prefix        = "${var.name_prefix}"
  nat_count          = "${var.nat_count}"
  public_subnet_ids  = ["${module.subnets.public_ids}"]
  private_subnet_ids = ["${module.subnets.private_ids}"]
  extra_tags         = "${var.extra_tags}"
}
