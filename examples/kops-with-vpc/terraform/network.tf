module "vpc" {
  source      = "../../../modules/vpc"
  region      = "${var.aws_region}"
  cidr        = "${var.vpc_cidr}"
  name_prefix = "${var.name}"
  extra_tags  = "${merge(var.extra_tags,
                   map("kubernetes.io/cluster/${var.kubernetes_cluster_name}", "shared"))}"
}

module "public-subnets" {
  source      = "../../../modules/subnets"
  azs         = ["${split(",", var.aws_availability_zones)}"]
  vpc_id      = "${module.vpc.vpc_id}"
  name_prefix = "${var.name}-kube-public"
  cidr_blocks = "${var.public_subnet_cidrs}"
  extra_tags  = "${merge(var.extra_tags,
                   map("kubernetes.io/cluster/${var.kubernetes_cluster_name}", "shared"),
                   map("kubernetes.io/role/elb", "1"))}"
}

module "public-gateway" {
  source            = "../../../modules/route-public"
  vpc_id            = "${module.vpc.vpc_id}"
  name_prefix       = "${var.name}-public"
  extra_tags        = "${var.extra_tags}"
  public_subnet_ids = ["${concat(module.public-subnets.ids)}"]
}
