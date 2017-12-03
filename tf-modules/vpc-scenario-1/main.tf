/**
 * ## AWS VPC, Scenario 1: VPC w/ Public Subnets
 *
 * This module creates a VPC with public subnets across one or more availability
 * zones, an internet gateway, and a route table for those subnets to pass
 * traffic through the gateway.
 *
 * Scenario 1 from AWS docs:
 * http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Scenarios.html
 *
 * Note that, when using this module and deploying the VPC for the first time,
 * Terraform needs the user to add the VPC, Subnets, and then Route Tables. For
 * example, use targets to apply these updates sequentially:
 *
 *     ᐅ tfp -out=tf.out -target=module.vpc.module.vpc
 *     ᐅ tfa tf.out
 *     ᐅ tfp -out=tf.out -target=module.vpc.module.public-subnets
 *     ᐅ tfa tf.out
 *     ᐅ tfp -out=tf.out -target=module.vpc.module.public-gateway
 *     ᐅ tfa tf.out
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
  cidr_blocks = ["${var.public_subnet_cidrs}"]
  extra_tags  = "${var.extra_tags}"
}

module "public-gateway" {
  source            = "../route-public"
  vpc_id            = "${module.vpc.vpc_id}"
  name_prefix       = "${var.name_prefix}-public"
  extra_tags        = "${var.extra_tags}"
  public_subnet_ids = ["${module.public-subnets.ids}"]
}
