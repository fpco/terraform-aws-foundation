module "kube-controller-iam" {
  source = "../../modules/kube-controller-iam"

  name_prefix = "${var.name}"
}

module "kube-worker-iam" {
  source = "../../modules/kube-worker-iam"

  name_prefix = "${var.name}"
}

module "kube-cluster" {
  source                 = "../../modules/kube-stack"
  availability_zones     = ["${slice(data.aws_availability_zones.available.names, 0, 2)}"]
  name_prefix            = "${var.name}"
  key_name               = "${aws_key_pair.main.key_name}"
  private_load_balancer  = false
  lb_subnet_ids          = ["${module.vpc.public_subnet_ids}"]
  worker_iam_profile     = "${module.kube-worker-iam.aws_iam_instance_profile_name}"
  controller_iam_profile = "${module.kube-controller-iam.aws_iam_instance_profile_name}"

  lb_security_group_ids = [
    "${module.kube-load-balancer-sg.id}",
  ]

  controller_ami        = "${var.coreos_stable_ami}"
  controller_subnet_ids = ["${module.vpc.private_subnet_ids}"]
  worker_ami            = "${var.coreos_stable_ami}"
  worker_subnet_ids     = ["${module.vpc.private_subnet_ids}"]

  controller_security_group_ids = [
    "${module.kube-controller-sg.id}",
  ]

  worker_security_group_ids = [
    "${module.kube-worker-sg.id}",
  ]
}

# security group for kube controller
module "kube-controller-sg" {
  source      = "../../modules/security-group-base"
  name        = "${var.name}-kube-controller"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the kube controllers in ${var.name}"
}

module "kube-controller-private-ssh-rule" {
  source            = "../../modules/ssh-sg"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${module.kube-controller-sg.id}"
}

module "kube-controller-kube-api-rule" {
  source            = "../../modules/single-port-sg"
  port              = "6443"
  protocol          = "tcp"
  description       = "Allow access to kube api from hosts in ${var.name} VPC"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${module.kube-controller-sg.id}"
}

module "kube-controller-etcd-rule" {
  source            = "../../modules/etcd-server-sg"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${module.kube-controller-sg.id}"
}

module "kube-controller-open-egress-rule" {
  source            = "../../modules/open-egress-sg"
  security_group_id = "${module.kube-controller-sg.id}"
}

# security group for kube worker
module "kube-worker-sg" {
  source      = "../../modules/security-group-base"
  name        = "${var.name}-kube-worker"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the kube workers in ${var.name}"
}

# allow ingress on any port, to kube workers, from any host in the VPC
module "kube-worker-open-ingress-rule" {
  source            = "../../modules/open-ingress-sg"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${module.kube-worker-sg.id}"
}

module "kube-worker-open-egress-rule" {
  source            = "../../modules/open-egress-sg"
  security_group_id = "${module.kube-worker-sg.id}"
}
# security group for load balancer
module "kube-load-balancer-sg" {
  source      = "../../modules/security-group-base"
  name        = "${var.name}-kube-load-balancer"
  vpc_id      = "${module.vpc.vpc_id}"
  description = "Security group for the kube load-balancer in ${var.name}"
}

module "kube-load-balancer-api-rule" {
  source            = "../../modules/single-port-sg"
  port              = "443"
  description       = "Public ingress to ELB for Kubernetes API controller, port 443"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${module.kube-load-balancer-sg.id}"
}

module "kube-load-balancer-open-egress-rule" {
  source            = "../../modules/open-egress-sg"
  security_group_id = "${module.kube-load-balancer-sg.id}"
}
