provider "aws" {
  region = "${var.region}"
}

resource "aws_key_pair" "main" {
  key_name   = "${var.name}"
  public_key = "${file(var.ssh_pubkey)}"
}

module "vpc" {
  source      = "../../modules/vpc-scenario-1"
  region      = "${var.region}"
  cidr        = "${var.vpc_cidr}"
  name_prefix = "${var.name}"

  #extra_tags           = "${var.extra_tags}"
  public_subnet_cidrs = ["${var.public_subnet_cidrs}"]
  azs                 = ["${slice(data.aws_availability_zones.available.names, 0, 2)}"]
}

module "kube-controller-iam" {
  source = "../../modules/kube-controller-iam"

  name_prefix = "${var.name}"
}

module "kube-worker-iam" {
  source = "../../modules/kube-worker-iam"

  name_prefix = "${var.name}"
}

resource "aws_security_group" "private-ssh" {
  name   = "${var.name}-private"
  vpc_id = "${module.vpc.vpc_id}"
}

module "private-ssh-sg" {
  source = "../../modules/ssh-sg"

  security_group_id = "${aws_security_group.private-ssh.id}"
  cidr_blocks       = ["${var.vpc_cidr}"]
}

# shared security group for SSH - for public subnet (access from EVERYWHERE)
# Think twice before associating this to an instance. Do you need it? Likely not.
resource "aws_security_group" "public-ssh" {
  name   = "${var.name}-public"
  vpc_id = "${module.vpc.vpc_id}"
}

module "public-ssh-sg" {
  source = "../../modules/ssh-sg"

  security_group_id = "${aws_security_group.public-ssh.id}"
  cidr_blocks       = ["0.0.0.0/0"]
}

# shared security group, open egress (outbound from nodes)
resource "aws_security_group" "open-egress" {
  name   = "open-egress"
  vpc_id = "${module.vpc.vpc_id}"
}

module "open-egress-sg" {
  source = "../../modules/open-egress-sg"

  security_group_id = "${aws_security_group.open-egress.id}"
}

# shared security group, ping responses from requesters in corporate CIDR ranges
resource "aws_security_group" "ping-respond" {
  name        = "ping-respond"
  description = "Respond to ping requests"
  vpc_id      = "${module.vpc.vpc_id}"
}

module "ping-respond-sg" {
  source = "../../modules/ping-respond-sg"

  security_group_id = "${aws_security_group.ping-respond.id}"
  cidr_blocks       = ["${var.vpc_cidr}"]
}

resource "aws_security_group" "kubernetes-worker" {
  vpc_id = "${module.vpc.vpc_id}"
}

resource "aws_security_group" "kubernetes-controller" {
  vpc_id = "${module.vpc.vpc_id}"
}

resource "aws_security_group" "kubernetes-load-balancer" {
  vpc_id = "${module.vpc.vpc_id}"
}

module "etcd-server-sg" {
  source = "../../modules/etcd-server-sg"

  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${aws_security_group.kubernetes-controller.id}"
}

module "kubernetes-controller-sg" {
  source = "../../modules/single-port-sg"

  port              = "6443"
  description       = "Kubernetes controller, port 6443"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${aws_security_group.kubernetes-controller.id}"
}

module "kubernetes-load-balancer-sg" {
  source = "../../modules/single-port-sg"

  port              = "443"
  description       = "Kubernetes controller, port 443"
  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${aws_security_group.kubernetes-load-balancer.id}"
}

module "kubernetes-worker-open-access-sg" {
  source = "../../modules/open-ingress-sg"

  cidr_blocks       = ["${var.vpc_cidr}"]
  security_group_id = "${aws_security_group.kubernetes-worker.id}"
}
