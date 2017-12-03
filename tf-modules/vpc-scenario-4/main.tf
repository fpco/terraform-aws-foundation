/**
 * ## AWS VPC, Scenario 4: VPC w/ Private Subnets, and VPN only
 *
 * This module creates a VPC with private subnets across one or more availability
 * zones, a hardware VPN gateway, and a route table for those subnets to pass
 * traffic through the gateway. No NAT or public subnets/gateway.
 *
 * Scenario 4 from AWS docs:
 * http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Scenarios.html
 *
 * Note that, when using this module and deploying the VPC for the first time,
 * Terraform needs the user to add the VPC, Subnets, and then Route Tables. For
 * example, use targets to apply these updates sequentially:
 *
 * ```
 * ᐅ terraform plan -out=tf.out -target=module.vpc.module.vpc
 * ᐅ terraform apply tf.out
 * ᐅ terraform plan -out=tf.out -target=module.vpc.module.private-subnets
 * ᐅ terraform apply tf.out
 * ```
 *
 * This module is not as well tested as the other modules, let us know how it
 * works for you.
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

module "private-subnets" {
  source      = "../subnets"
  azs         = "${var.azs}"
  vpc_id      = "${module.vpc.vpc_id}"
  public      = false
  name_prefix = "${var.name_prefix}-private"
  cidr_blocks = "${var.private_subnet_cidrs}"
  extra_tags  = "${var.extra_tags}"
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
  vpc_id           = "${module.vpc.vpc_id}"
  extra_tags       = "${var.extra_tags}"
  remote_device_ip = "${var.vpn_remote_ip}"
  static_routes    = ["${var.vpn_static_routes}"]
  name             = "${var.name_prefix}-vpn"
}
