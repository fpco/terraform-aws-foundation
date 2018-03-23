/**
 * ## Kube Load Balancer Security Group
 *
 * Define a security group for kube load balancer
 *
 */

module "kube-load-balancer-sg" {
  source      = "../security-group-base"
  name        = "${var.name}"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the kube load-balancer in ${var.name}"
}

module "kube-load-balancer-api-rule" {
  source            = "../single-port-sg"
  port              = "443"
  description       = "Public ingress to ELB for Kubernetes API controller, port 443"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${module.kube-load-balancer-sg.id}"
}

module "kube-load-balancer-open-egress-rule" {
  source            = "../open-egress-sg"
  security_group_id = "${module.kube-load-balancer-sg.id}"
}
