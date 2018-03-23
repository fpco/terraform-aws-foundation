/**
 * ## Kube Worker Security Group
 *
 * Define a security group for kube worker
 *
 */

variable "cidr_blocks" {
  description = "List of CIDR block ranges that the SG allows ingress from"
  type        = "list"
}

variable "name" {
  description = "Name of the Kube Controller security group"
}

# security group for kube worker
module "kube-worker-sg" {
  source      = "../security-group-base"
  name        = "${var.name}-kube-worker"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the kube workers in ${var.name}"
}

# allow ingress on any port, to kube workers, from any host in the VPC
module "kube-worker-open-ingress-rule" {
  source            = "../open-ingress-sg"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${module.kube-worker-sg.id}"
}

module "kube-worker-open-egress-rule" {
  source            = "../open-egress-sg"
  security_group_id = "${module.kube-worker-sg.id}"
}
