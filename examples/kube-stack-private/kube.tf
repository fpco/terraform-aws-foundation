module "kube-controller-iam" {
  source      = "../../modules/kube-controller-iam"
  name_prefix = var.name
}

module "kube-worker-iam" {
  source      = "../../modules/kube-worker-iam"
  name_prefix = var.name
}

# we should pipe `extra_tags` variable through this to controller / worker inputs
# for this module (TODO)
module "kube-cluster" {
  source                 = "../../modules/kube-stack"
  availability_zones     = local.azs
  name_prefix            = var.name
  key_name               = aws_key_pair.main.key_name
  private_load_balancer  = false
  lb_subnet_ids          = module.vpc.public_subnet_ids
  worker_iam_profile     = module.kube-worker-iam.aws_iam_instance_profile_name
  controller_iam_profile = module.kube-controller-iam.aws_iam_instance_profile_name
  controller_ami         = var.coreos_stable_ami_id
  controller_subnet_ids  = module.vpc.private_subnet_ids
  worker_ami             = var.coreos_stable_ami_id
  worker_subnet_ids      = module.vpc.private_subnet_ids

  controller_security_group_ids = [
    module.kube-controller-sg.id,
  ]

  worker_security_group_ids = [
    module.kube-worker-sg.id,
  ]

  lb_security_group_ids = [
    module.kube-load-balancer-sg.id,
  ]
}

# security group for kube controller nodes
module "kube-controller-sg" {
  source           = "../../modules/kube-controller-sg"
  name_prefix      = var.name
  vpc_id           = module.vpc.vpc_id
  cidr_blocks_api  = [var.vpc_cidr]
  cidr_blocks_ssh  = [var.vpc_cidr]
  cidr_blocks_etcd = [var.vpc_cidr]
}

# security group for kube worker nodes
module "kube-worker-sg" {
  source          = "../../modules/kube-worker-sg"
  name_prefix     = var.name
  vpc_id          = module.vpc.vpc_id
  cidr_blocks_ssh = [var.vpc_cidr]

  # only allow open access to worker nodes in the VPC
  cidr_blocks_open_ingress = [var.vpc_cidr]
}

# security group for kube load balancer (ELB)
module "kube-load-balancer-sg" {
  source          = "../../modules/kube-load-balancer-sg"
  name_prefix     = var.name
  vpc_id          = module.vpc.vpc_id
  cidr_blocks_api = [var.vpc_cidr]
}

