/**
 *## AWS VPC, Scenario 1: VPC w/ Public Subnets
 *
 * This module creates a VPC with public subnets across one or more availability
 * zones, an internet gateway, and a route table for those subnets to pass
 * traffic through the gateway.
 *
 * Scenario 1 from AWS docs:
 * http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Scenarios.html
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
  vpc_id      = "${module.vpc.id}"
  name_prefix = "${var.name_prefix}-public"
  cidr_blocks = "${var.public_subnet_cidrs}"
  extra_tags  = "${var.extra_tags}"
}

module "public-gateway" {
  source      = "../route-public"
  vpc_id      = "${module.vpc.id}"
  name_prefix = "${var.name_prefix}-public"
  extra_tags  = "${var.extra_tags}"
  public_subnet_ids = ["${module.public-subnets.ids}"]
}
