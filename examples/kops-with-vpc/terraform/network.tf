module "vpc" {
  source      = "../../../modules/vpc"
  region      = var.region
  cidr        = var.vpc_cidr
  name_prefix = "${var.name_prefix}-vpc"
  extra_tags = merge(
    var.extra_tags,
    {
      "kubernetes.io/cluster/${var.kubernetes_cluster_name}" = "shared"
    },
  )
}

module "public-subnets" {
  source      = "../../../modules/subnets"
  azs         = split(",", var.availability_zones)
  vpc_id      = module.vpc.vpc_id
  name_prefix = "${var.name_prefix}-subnets"
  cidr_blocks = split(",", var.public_subnet_cidrs)
  extra_tags = merge(
    var.extra_tags,
    {
      "kubernetes.io/cluster/${var.kubernetes_cluster_name}" = "shared"
    },
    {
      "kubernetes.io/role/elb" = "1"
    },
  )
}

module "public-gateway" {
  source            = "../../../modules/route-public"
  vpc_id            = module.vpc.vpc_id
  name_prefix       = "${var.name_prefix}-route-public"
  extra_tags        = var.extra_tags
  public_subnet_ids = [concat(module.public-subnets.ids)]
}

