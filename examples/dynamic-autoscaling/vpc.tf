module "vpc" {
  source      = "../../modules/vpc"
  cidr        = "${var.vpc["cidr"]}"
  name_prefix = "${var.name}"
  region      = "${var.region}"
}

module "public-subnets" {
  source      = "../../modules/subnets"
  name_prefix = "${var.name}"
  public      = true
  vpc_id      = "${module.vpc.vpc_id}"
  azs         = "${data.aws_availability_zones.available.names}"

  cidr_blocks = [
    "${cidrsubnet(module.vpc.vpc_cidr_block, 8, 1)}",
    "${cidrsubnet(module.vpc.vpc_cidr_block, 8, 2)}",
    "${cidrsubnet(module.vpc.vpc_cidr_block, 8, 3)}",
  ]
}

module "route-public" {
  source            = "../../modules/route-public"
  name_prefix       = "${var.name}"
  vpc_id            = "${module.vpc.vpc_id}"
  public_subnet_ids = "${module.public-subnets.ids}"
}
