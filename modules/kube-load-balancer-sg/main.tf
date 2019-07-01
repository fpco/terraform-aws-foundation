module "kube-load-balancer-sg" {
  source      = "../security-group-base"
  name        = "${var.name_prefix}-${var.name_suffix}"
  vpc_id      = var.vpc_id
  description = "Security group for the ${var.name_prefix} kube load-balancer"
}

module "api-rule" {
  source            = "../single-port-sg"
  port              = var.api_port
  description       = "Ingress thru ELB for Kubernetes API, on port ${var.api_port}"
  cidr_blocks       = var.cidr_blocks_api
  security_group_id = module.kube-load-balancer-sg.id
}

module "open-egress-rule" {
  source            = "../open-egress-sg"
  security_group_id = module.kube-load-balancer-sg.id
}

