/**
 * ## Autoscaling Clusters (built on Consul)
 * 
 * **DEPRECATED** - This module will be removed in a future release.
 * 
 * This module reuses two other modules in this suite:
 * 
 * * `asg`
 * * `cluster-net`
 * 
 * The purpose of this module is to pair the ASG with subnets created specifically
 * for the ASG. Similarly, the individual parts (`asg` / `cluster-net`), exist to
 * allow flexibility, and to enable user-developers with the option of putting a
 * new ASG into an existing network, and upfront or further down the line.
 * 
 * NOTES:
 * 
 * * this should be updated to support N number of subnets
 * * need to plumb in support for `public_ip` and `elb`
 * 
 * ### Example
 * 
 * ```
 * # cluster of "workers", built on a cluster of consul agents
 * module "cworkers-a" {
 *     source = "../tf-modules/consul-cluster"
 *     ami = "${var.ami}"
 *     name = "${var.name}"
 *     max_nodes = 5
 *     min_nodes = 3
 *     desired_capacity = 3
 *     key_name = "${aws_key_pair.tests.key_name}"
 *     region = "${var.region}"
 *     cidr_minions_a = "${var.cidr_minions_a}"
 *     cidr_minions_c = "${var.cidr_minions_c}"
 *     vpc_id = "${module.test-vpc.id}"
 *     route_table_id = "${module.test-vpc.route_table_id}"
 *     cluster_security_group_ids = "${module.management-cluster.nomad_agent_sg}, ${module.consul-agent-sg.id}, ${aws_security_group.worker-service.id}, ${module.public-ssh-sg.id}"
 *     user_data = "${module.worker-init.user_data}"
 * }
 * ```
 */

# a consul agent cluster is an auto-scaling group..
module "agent-asg" {
  source             = "../asg"
  ami                = "${var.ami}"
  azs                = ["${var.region}a", "${var.region}c"]
  desired_capacity   = "${var.desired_capacity}"
  elb_names          = ["${var.load_balancers}"]
  instance_type      = "${var.instance_type}"
  max_nodes          = "${var.max_nodes}"
  min_nodes          = "${var.min_nodes}"
  key_name           = "${var.key_name}"
  name_prefix        = "${var.name}"
  name_suffix        = "cluster"
  subnet_ids         = ["${module.cluster-net.id_a}", "${module.cluster-net.id_c}"]
  security_group_ids = ["${var.cluster_security_group_ids}"]
  user_data          = "${var.user_data}"
  root_volume_type   = "${var.root_volume_type}"
  root_volume_size   = "${var.root_volume_size}"
}

# and a multi-az network
module "cluster-net" {
  source         = "../cluster-network"
  name           = "${var.name}"
  route_table_id = "${var.route_table_id}"
  vpc_id         = "${var.vpc_id}"
  cidr_a         = "${var.cidr_minions_a}"
  cidr_c         = "${var.cidr_minions_c}"
  region         = "${var.region}"
  public_ip      = "${var.public_ip}"
}
