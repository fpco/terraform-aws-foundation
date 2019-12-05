module "vpc1-ad" {
  source      = "../../modules/vpc-scenario-1"
  name_prefix = var.name
  region      = data.aws_region.current.name
  cidr        = var.vpc1_cidr
  azs         = [data.aws_availability_zones.available.names[0]]

  extra_tags = var.extra_tags

  public_subnet_cidrs = var.public_subnet_cidrs_vpc1
}

module "private-subnets" {
  source      = "../../modules/subnets"
  azs         = slice(data.aws_availability_zones.available.names, 0, 2)
  vpc_id      = module.vpc1-ad.vpc_id
  name_prefix = "${var.name}-private"
  cidr_blocks = var.private_subnet_cidrs_vpc1
  public      = false
  extra_tags  = merge(var.extra_tags, var.private_subnet_extra_tags)
}

module "nat-gateway" {
  source             = "../../modules/nat-gateways"
  vpc_id             = module.vpc1-ad.vpc_id
  name_prefix        = var.name
  nat_count          = length(var.public_subnet_cidrs_vpc1)
  public_subnet_ids  = module.vpc1-ad.public_subnet_ids
  private_subnet_ids = module.private-subnets.ids
  extra_tags         = merge(var.extra_tags, var.nat_gateway_extra_tags)
}

resource "aws_directory_service_directory" "main" {
  name     = local.domain
  password = var.active_directory_password
  size     = "Small"
  edition  = "Standard"
  type     = "MicrosoftAD"

  vpc_settings {
    vpc_id     = module.vpc1-ad.vpc_id
    subnet_ids = module.private-subnets.ids
  }
}
