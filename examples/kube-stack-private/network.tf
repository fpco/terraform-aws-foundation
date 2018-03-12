module "vpc" {
  source      = "../../modules/vpc-scenario-2"
  region      = "${var.region}"
  cidr        = "${var.vpc_cidr}"
  name_prefix = "${var.name}"

  public_subnet_cidrs  = ["${var.public_subnet_cidrs}"]
  private_subnet_cidrs = ["${var.private_subnet_cidrs}"]
  azs                  = ["${slice(data.aws_availability_zones.available.names, 0, 2)}"]
}
