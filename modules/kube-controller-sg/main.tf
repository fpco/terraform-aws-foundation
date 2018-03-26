/**
 * ## Kube Controller Security Group
 *
 * Define a security group for kube controller
 *
 * provisioning for kube controller sg example:
 * module "kube-controller-sg" {
 *  source                    = "../../modules/kube-controller-sg"
 *  name_prefix               = "${var.name}"
 *  name_suffix               = "kube-controller"
 *  vpc_id                    = "${module.vpc.vpc_id}"
 *  vpc_cidr                  = "${var.vpc_cidr}"
 *  vpc_cidr_controller_api   = "${var.vpc_cidr_controller_api}"
 *  vpc_cidr_controller_ssh   = "${var.vpc_cidr_controller_ssh}"
 *  vpc_cidr_controller_etcd  = "${var.vpc_cidr_controller_etcd}"
 * }
 *
 */

module "kube-controller-sg" {
  source      = "../security-group-base"
  name        = "${var.name_prefix}-${var.name_suffix}"
  vpc_id      = "${var.vpc_id}"
  description = "Security group for the kube controllers in ${var.name}"
}

module "api-rule" {
  source            = "../single-port-sg"
  port              = "6443"
  protocol          = "tcp"
  description       = "Allow access to kube api from hosts in ${var.name} VPC"
  cidr_blocks       = ["${var.vpc_cidr_controller_api}"]
  security_group_id = "${module.kube-controller-sg.id}"
}

module "private-ssh-rule" {
  source            = "../ssh-sg"
  cidr_blocks       = ["${var.vpc_cidr_controller_ssh}"]
  security_group_id = "${module.kube-controller-sg.id}"
}

module "etcd-rule" {
  source            = "../etcd-server-sg"
  cidr_blocks       = ["${var.vpc_cidr_controller_etcd}"]
  security_group_id = "${module.kube-controller-sg.id}"
}

module "open-egress-rule" {
  source            = "../open-egress-sg"
  security_group_id = "${module.kube-controller-sg.id}"
}
