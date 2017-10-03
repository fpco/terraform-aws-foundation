module "etcd-server-sg" {
  source      = "../../tf-modules/etcd-server-sg"
  name_prefix = "${var.name}"
  vpc_id      = "${module.vpc.vpc_id}"
  cidr_blocks = ["${var.vpc_cidr}"]
}

module "kube-controller-sg" {
  source      = "../../tf-modules/single-port-tcp-sg"
  port        = "6443"
  name_prefix = "${var.name}"
  name_suffix = "kube-controller"
  vpc_id      = "${module.vpc.vpc_id}"
  cidr_blocks = ["${var.vpc_cidr}"]
}

module "kube-controller-lb-sg" {
  source      = "../../tf-modules/single-port-tcp-sg"
  port        = "443"
  name_prefix = "${var.name}"
  name_suffix = "kube-controller-lb"
  vpc_id      = "${module.vpc.vpc_id}"
  cidr_blocks = ["0.0.0.0/0"]
}

module "kube-cluster" {
  source                = "../../tf-modules/kube-stack"
  availability_zones    = ["${slice(data.aws_availability_zones.available.names, 0, 2)}"]
  name_prefix           = "${var.name}"
  key_name              = "${aws_key_pair.main.key_name}"
  private_load_balancer = false
  lb_subnet_ids         = ["${module.vpc.public_subnet_ids}"]
  lb_security_group_ids = [
    "${module.kube-controller-lb-sg.id}",
    "${module.open-egress-sg.id}",
  ]
  #controller_ami        = "${data.aws_ami.coreos-stable.image_id}"
  controller_ami        = "ami-d88605b9"
  controller_subnet_ids = ["${module.vpc.public_subnet_ids}"]
  #worker_ami            = "${data.aws_ami.coreos-stable.image_id}"
  worker_ami            = "ami-d88605b9"
  worker_subnet_ids     = ["${module.vpc.public_subnet_ids}"]

  controller_security_group_ids = [
    "${module.public-ssh-sg.id}",
    "${module.open-egress-sg.id}",
    "${module.etcd-server-sg.id}",
    "${module.kube-controller-sg.id}",
  ]

  worker_security_group_ids = [
    "${module.public-ssh-sg.id}",
    "${module.open-egress-sg.id}",
  ]
}
