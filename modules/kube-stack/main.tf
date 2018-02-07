/**
 * ## Kube-Stack (w/ Terraform and kubespray)
 *
 * DOCUMENT.
 *
 * ### Assumptions
 *
 * Here is a list of assumptions this module makes:
 *
 * * the AMIs used are based on CoreOS
 * * the caller provides the list of security group ids, and subnets
 *
 *
 * ### Example - how to use this module
 *
 * ```
 *     module "etcd-server-sg" {
 *       source      = "../etcd-server-sg"
 *       name_prefix = "${var.name}"
 *       vpc_id      = "${module.vpc.vpc_id}"
 *       cidr_blocks = ["${var.vpc_cidr}"]
 *     }
 *     
 *     module "kube-controller-sg" {
 *       source      = "../single-port-tcp-sg"
 *       port        = "6443"
 *       name_prefix = "${var.name}"
 *       name_suffix = "kube-controller"
 *       vpc_id      = "${module.vpc.vpc_id}"
 *       cidr_blocks = ["${var.vpc_cidr}"]
 *     }
 *
 *
 *      "${module.private-ssh-sg.id}",
 *      "${module.open-egress-sg.id}",
 *      "${module.etcd-server-sg.id}",
 *      "${module.kube-controller-sg.id}",
 * ```
 */

# an auto-scaling group for kube controllers
module "controller-asg" {
  source             = "../asg"
  azs                = ["${var.availability_zones}"]
  ami                = "${var.controller_ami}"
  name_prefix        = "${var.name_prefix == "" ? "" : "${var.name_prefix}"}"
  elb_names          = ["${aws_elb.kube-controllers.name}"]
  iam_profile        = "${var.controller_iam_profile}"
  instance_type      = "${var.controller_instance_type}"
  root_volume_type   = "${var.controller_root_volume_type}"
  root_volume_size   = "${var.controller_root_volume_size}"
  desired_capacity   = "${var.controller_desired_capacity}"
  max_nodes          = "${var.controller_max_nodes}"
  min_nodes          = "${var.controller_min_nodes}"
  key_name           = "${var.key_name}"
  name_suffix        = "${var.controller_name_suffix}"
  public_ip          = "${var.controller_public_ip}"
  subnet_ids         = ["${var.controller_subnet_ids}"]
  security_group_ids = ["${var.controller_security_group_ids}"]

  extra_tags = ["${list(
    map("key", "kubespray-role",
        "value", "kube-master,etcd,kube-node",
	"propagate_at_launch", true),
    map("key", "cluster-name",
        "value", "${var.name_prefix}",
	"propagate_at_launch", true),
    )}"]

  user_data = <<END_INIT
#!/bin/bash
# update the node's hostname
HN_PREFIX="${var.controller_name_suffix}"
OLDHN="$(hostname -s)"
HOSTNAME=$HN_PREFIX-$OLDHN
hostnamectl set-hostname $HOSTNAME
END_INIT
}

# an auto-scaling group for kube workers
module "worker-asg" {
  source             = "../asg"
  azs                = ["${var.availability_zones}"]
  ami                = "${var.worker_ami}"
  name_prefix        = "${var.name_prefix == "" ? "" : "${var.name_prefix}"}"
  iam_profile        = "${var.worker_iam_profile}"
  instance_type      = "${var.worker_instance_type}"
  root_volume_type   = "${var.worker_root_volume_type}"
  root_volume_size   = "${var.worker_root_volume_size}"
  desired_capacity   = "${var.worker_desired_capacity}"
  max_nodes          = "${var.worker_max_nodes}"
  min_nodes          = "${var.worker_min_nodes}"
  key_name           = "${var.key_name}"
  name_suffix        = "${var.worker_name_suffix}"
  public_ip          = "${var.worker_public_ip}"
  subnet_ids         = ["${var.worker_subnet_ids}"]
  security_group_ids = ["${var.worker_security_group_ids}"]

  extra_tags = ["${list(
    map("key", "kubespray-role",
        "value", "kube-node",
	"propagate_at_launch", true),
    map("key", "cluster-name",
        "value", "${var.name_prefix}",
	"propagate_at_launch", true),
    )}"]

  user_data = <<END_INIT
#!/bin/bash
# update the node's hostname
HN_PREFIX="${var.worker_name_suffix}"
OLDHN="$(hostname -s)"
HOSTNAME=$HN_PREFIX-$OLDHN
hostnamectl set-hostname $HOSTNAME
END_INIT
}
