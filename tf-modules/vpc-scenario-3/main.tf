/**
 * ## AWS VPC, Scenario 3: VPC w/ Public, Private Subnets and a Hardware VPN
 *
 * This module creates a VPC with both public and private subnets spanning one or
 * more availability zones, an internet gateway for the public subnets and a
 * hardware VPN gateway for the private subnets.
 *
 * Scenario 3 from AWS docs:
 * http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Scenarios.html
 *
 * Note that, when using this module and deploying the VPC for the first time,
 * Terraform needs the user to add the VPC, Subnets, and then Route Tables. For
 * example, use targets to apply these updates sequentially:
 *
 *     ᐅ terraform plan -out=tf.out -target=module.vpc.module.vpc
 *     ᐅ terraform apply tf.out
 *     ᐅ terraform plan -out=tf.out -target=module.vpc.module.public-subnets
 *     ᐅ terraform apply tf.out
 *     ᐅ terraform plan -out=tf.out -target=module.vpc.module.public-gateway
 *     ᐅ terraform apply tf.out
 *     ᐅ terraform plan -out=tf.out -target=module.vpc.module.private-subnets
 *     ᐅ terraform apply tf.out
 *
 */

module "vpc" {
  source               = "../vpc"
  region               = "${var.region}"
  cidr                 = "${var.cidr}"
  name_prefix          = "${var.name_prefix}"
  enable_dns_hostnames = "${var.enable_dns_hostnames}"
  enable_dns_support   = "${var.enable_dns_support}"
  dns_servers          = ["${var.dns_servers}"]
  extra_tags           = "${var.extra_tags}"
}

module "public-subnets" {
  source      = "../subnets"
  azs         = "${var.azs}"
  vpc_id      = "${module.vpc.vpc_id}"
  name_prefix = "${var.name_prefix}-public"
  cidr_blocks = "${var.public_subnet_cidrs}"
  extra_tags  = "${var.extra_tags}"
}

module "public-gateway" {
  source            = "../route-public"
  vpc_id            = "${module.vpc.vpc_id}"
  name_prefix       = "${var.name_prefix}-public"
  extra_tags        = "${var.extra_tags}"
  public_subnet_ids = ["${module.public-subnets.ids}"]
}

module "private-subnets" {
  source      = "../subnets"
  azs         = "${var.azs}"
  vpc_id      = "${module.vpc.vpc_id}"
  public      = false
  name_prefix = "${var.name_prefix}-private"
  cidr_blocks = "${var.private_subnet_cidrs}"
  extra_tags  = "${var.extra_tags}"
}

module "nat-gateway" {
  source             = "../nat-gateways"
  vpc_id             = "${module.vpc.vpc_id}"
  name_prefix        = "${var.name_prefix}"
  nat_count          = "${length(module.private-subnets.ids)}"
  public_subnet_ids  = ["${module.public-subnets.ids}"]
  private_subnet_ids = ["${module.private-subnets.ids}"]
  extra_tags         = "${var.extra_tags}"
}

## TODO: Move these next two resources into the IPSEC/VPN module..?
# Route private subnets through the VPN gateway
resource "aws_route_table" "private-vpn" {
  vpc_id           = "${module.vpc.vpc_id}"
  propagating_vgws = ["${module.vpn.vpn_gw_id}"]

  tags = {
    Name = "${var.name_prefix}-private-vpn"
  }
}

resource "aws_route_table_association" "private-vpn" {
  count          = "${length(module.private-subnets.ids)}"
  subnet_id      = "${element(module.private-subnets.ids, count.index)}"
  route_table_id = "${aws_route_table.private-vpn.id}"
}

module "vpn" {
  source           = "../aws-ipsec-vpn"
  name             = "${var.name_prefix}"
  vpc_id           = "${module.vpc.vpc_id}"
  extra_tags       = "${var.extra_tags}"
  remote_device_ip = "${var.vpn_remote_ip}"
  static_routes    = ["${var.vpn_static_routes}"]
}
