module "kube-worker-sg" {
  source      = "../security-group-base"
  name        = "${var.name_prefix}-${var.name_suffix}"
  vpc_id      = var.vpc_id
  description = "Security group for the ${var.name_prefix} kube worker nodes"
}

module "private-ssh-rule" {
  source            = "../ssh-sg"
  cidr_blocks       = var.cidr_blocks_ssh
  security_group_id = module.kube-worker-sg.id
}

# allow ingress on any port, to kube workers, from any host in the list of CIDR blocks
module "open-ingress-rule" {
  source            = "../open-ingress-sg"
  cidr_blocks       = var.cidr_blocks_open_ingress
  security_group_id = module.kube-worker-sg.id
}

module "open-egress-rule" {
  source            = "../open-egress-sg"
  security_group_id = module.kube-worker-sg.id
}

