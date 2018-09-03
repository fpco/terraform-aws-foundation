module "vpc" {
  source      = "fpco/foundation/aws//modules/vpc"
  version     = "0.7.5"
  region      = "${var.region}"
  cidr        = "${var.vpc_cidr}"
  name_prefix = "${var.name}"
  extra_tags  = "${merge(var.extra_tags,
                   map("kubernetes.io/cluster/${var.kubernetes_cluster_name}", "shared"))}"
}

module "public-subnets" {
  source      = "fpco/foundation/aws//modules/subnets"
  version     = "0.7.5"
  azs         = "${var.aws_availability_zones}"
  vpc_id      = "${module.vpc.vpc_id}"
  name_prefix = "${var.name}-kube-public"
  cidr_blocks = "${var.kube_public_subnet_cidrs}"
  extra_tags  = "${merge(var.extra_tags,
                   map("kubernetes.io/cluster/${var.kubernetes_cluster_name}", "shared"),
                   map("kubernetes.io/role/elb", "1"))}"
}

module "public-gateway" {
  source            = "fpco/foundation/aws//modules/route-public"
  version           = "0.7.5"
  vpc_id            = "${module.vpc.vpc_id}"
  name_prefix       = "${var.name}-public"
  extra_tags        = "${var.extra_tags}"
  public_subnet_ids = ["${concat(module.public-subnets.ids)}"]
}
