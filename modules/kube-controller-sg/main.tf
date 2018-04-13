module "kube-controller-sg" {
  source      = "../security-group-base"
  name        = "${var.name_prefix}-${var.name_suffix}"
  vpc_id      = "${var.vpc_id}"
  description = "Security group for the ${var.name_prefix} kube controller nodes"
}

module "api-rule" {
  source            = "../single-port-sg"
  port              = "${var.api_port}"
  protocol          = "tcp"
  description       = "Allow access to the kuberenets api"
  cidr_blocks       = ["${var.cidr_blocks_api}"]
  security_group_id = "${module.kube-controller-sg.id}"
}

module "private-ssh-rule" {
  source            = "../ssh-sg"
  cidr_blocks       = ["${var.cidr_blocks_ssh}"]
  security_group_id = "${module.kube-controller-sg.id}"
}

module "etcd-rule" {
  source            = "../etcd-server-sg"
  cidr_blocks       = ["${var.cidr_blocks_etcd}"]
  security_group_id = "${module.kube-controller-sg.id}"
}

module "open-egress-rule" {
  source            = "../open-egress-sg"
  security_group_id = "${module.kube-controller-sg.id}"
}
