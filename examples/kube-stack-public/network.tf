module "vpc" {
  source      = "../../modules/vpc-scenario-1"
  region      = var.region
  cidr        = var.vpc_cidr
  name_prefix = var.name

  #extra_tags           = "${var.extra_tags}"
  public_subnet_cidrs = var.public_subnet_cidrs
  azs                 = slice(data.aws_availability_zones.available.names, 0, 2)
}

