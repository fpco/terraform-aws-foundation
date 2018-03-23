/**
 * ## Kube Controller Security Group
 *
 * Define a security group for kube controller
 *
 */

variable "cidr_blocks" {
  description = "List of CIDR block ranges that the SG allows ingress from"
  type        = "list"
}

variable "name" {
  description = "Name of the Kube Controller security group"
}

module "kube-controller-sg" {
  source      = "../security-group-base"
  name        = "${var.name}"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the kube controllers in ${var.name}"
}

module "kube-controller-private-ssh-rule" {
  source            = "../ssh-sg"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${module.kube-controller-sg.id}"
}

module "kube-controller-kube-api-rule" {
  source            = "../single-port-sg"
  port              = "6443"
  protocol          = "tcp"
  description       = "Allow access to kube api from hosts in ${var.name} VPC"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${module.kube-controller-sg.id}"
}

module "kube-controller-etcd-rule" {
  source            = "../etcd-server-sg"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${module.kube-controller-sg.id}"
}

module "kube-controller-open-egress-rule" {
  source            = "../open-egress-sg"
  security_group_id = "${module.kube-controller-sg.id}"
}
