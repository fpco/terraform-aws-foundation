module "vpc" {
  source      = "../../modules/vpc-scenario-2"
  region      = var.region
  cidr        = var.vpc_cidr
  name_prefix = var.name
  extra_tags = merge(
    {
      "kubernetes.io/cluster/${var.name}" = "owned"
    },
    var.extra_tags,
  )

  public_subnet_extra_tags = merge(
    {
      "kubernetes.io/role/elb" = "true"
    },
  )

  public_subnet_cidrs  = var.public_subnet_cidrs
  private_subnet_cidrs = var.private_subnet_cidrs
  azs                  = local.azs
}

