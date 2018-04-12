/**
 * ## Kube Worker Security Group
 *
 * Define a security group for kube worker
 *
 * provisioning Kube worker sg example:
 * module "kube-worker-sg" {
 *   source                = "../../modules/kube-worker-sg"
 *   name_prefix           = "${var.name}"
 *   name_suffix           = "kube-worker"
 *   vpc_id                = "${module.vpc.vpc_id}"
 *   vpc_cidr              = "${var.vpc_cidr}"
 *   vpc_cidr_worker_ssh   = "${var.vpc_cidr_worker_ssh}"
 * }
 *
 */

module "kube-worker-sg" {
  source      = "../security-group-base"
  name        = "${var.name_prefix}-${var.name_suffix}"
  vpc_id      = "${var.vpc_id}"
  description = "Security group for the kube workers in ${var.name}"
}

module "private-ssh-rule" {
  source            = "../ssh-sg"
  cidr_blocks       = ["${var.vpc_cidr_worker_ssh}"]
  security_group_id = "${module.kube-worker-sg.id}"
}

# allow ingress on any port, to kube workers, from any host in the VPC
module "open-ingress-rule" {
  source            = "../open-ingress-sg"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${module.kube-worker-sg.id}"
}

module "open-egress-rule" {
  source            = "../open-egress-sg"
  security_group_id = "${module.kube-worker-sg.id}"
}
