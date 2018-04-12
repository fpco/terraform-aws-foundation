/**
 * ## Kube Load Balancer Security Group
 *
 * Define a security group for kube load balancer
 *
 * provisioning Kube Load Balancer example:
 * module "sg-lb" {
 *   source      = "../../modules/kube-loadbalancer-sg"
 *   name_prefix = "${var.name}"
 *   name_suffix = "kube-loadbalancer"
 *   vpc_id      = "${module.vpc.vpc_id}"
 *   vpc_cidr    = "${var.vpc_cidr}"
 * }
 *
 */

module "sg-lb" {
  source      = "../security-group-base"
  name        = "${var.name_prefix}-${var.name_suffix}"
  vpc_id      = "${var.vpc_id}"
  description = "Security group for the kube load-balancer in ${var.name}"
}

module "api-rule" {
  source            = "../single-port-sg"
  port              = "443"
  description       = "Public ingress to ELB for Kubernetes API controller, port 443"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${module.sg-lb.id}"
}

module "open-egress-rule" {
  source            = "../open-egress-sg"
  security_group_id = "${module.sg-lb.id}"
}
