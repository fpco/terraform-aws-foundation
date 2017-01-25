/**
 *##Deployment configuration for master nodes for Elasticsearh cluster
 *
 * Each master node has a persistent EBS volume created for. Each node is managed
 * by an auto scaling group, so if a helath check fails for an instance it will be
 * torn down and a new one deployed with the same EBS volume mounted.
 */

module "master-node-ebs-volumes" {
  source       = "../persistent-ebs-volumes"
  name_prefix  = "${var.name_prefix}-master-node"
  volume_count = "${var.master_node_count}"
  azs          = ["${var.vpc_azs}"]
  size         = "${var.master_node_ebs_size}"
  snapshot_ids = ["${var.master_node_snapshot_ids}"]
  encrypted    = "false"
  device_name  = "/dev/xvdf"
}

resource "aws_iam_role_policy_attachment" "master-node-attach-ebs-volume" {
  count = "${var.master_node_count}"
  role = "${element(aws_iam_role.master-node-role.*.name, count.index)}"
  policy_arn = "${element(module.master-node-ebs-volumes.iam_volume_policy_arns, count.index)}"
}

# Master nodes count cannot change through cluster lifetime without manually
# changing settings on existing nodes, namely changin `minimum_master_nodes` setting.
resource "aws_launch_configuration" "master-node-lc" {
  count                = "${var.master_node_count}"
  name_prefix          = "${var.name_prefix}-master-nodes-lc-${count.index}-"
  image_id             = "${var.node_ami}"
  instance_type        = "${var.master_node_instance_type}"
  iam_instance_profile = "${element(aws_iam_instance_profile.master-node-iam-profile.*.id, count.index)}"
  key_name             = "${var.key_name}"
  security_groups      = ["${aws_security_group.master-node-sg.id}"]
  user_data            = "${element(data.template_file.master-node-setup.*.rendered, count.index)}"
  lifecycle            = {
    create_before_destroy = true
  }
}

# A single master node autoscaling group.
resource "aws_autoscaling_group" "master-node-asg" {
  count                = "${var.master_node_count}"
  availability_zones   = ["${element(var.vpc_azs, count.index)}"]
  name                 = "${var.name_prefix}-master-node-asg-${count.index}-${element(var.vpc_azs, count.index)}"
  max_size             = 1
  min_size             = 1
  desired_capacity     = 1
  launch_configuration = "${element(aws_launch_configuration.master-node-lc.*.name, count.index)}"
  health_check_type    = "EC2"
  vpc_zone_identifier  = ["${element(var.vpc_private_subnet_ids, count.index)}"]
  lifecycle            = {
    create_before_destroy = true
  }

  tag = [{
    key                 = "Name"
    value               = "${var.name_prefix}-master-node-${count.index}-${element(var.vpc_azs, count.index)}"
    propagate_at_launch = true
  },{
    key                 = "cluster"
    value               = "${var.name_prefix}-elasticsearch-cluster"
    propagate_at_launch = true
  }]

}

# Set up script for each master node
data "template_file" "master-node-setup" {
  count    = "${var.master_node_count}"
  template = "${file("${path.module}/data/setup.tpl.sh")}"

  vars {
    mount_snippet = "${element(module.master-node-ebs-volumes.volume_mount_snippets, count.index)}"
    device_name   = "/dev/xvdf"
    mount_point   = "/mnt/elasticsearch"
    wait_interval = 1
    region        = "${var.region}"
    config_yaml   = "${element(data.template_file.master-node-config.*.rendered, count.index)}"
  }
}


# Elasticsearch configuration file for each master node
data "template_file" "master-node-config" {
  count    = "${var.master_node_count}"
  template = "${file("${path.module}/data/master_node_conf.tpl.yml")}"

  vars {
    node_name          = "${var.name_prefix}-master-node-${count.index}-${element(var.vpc_azs, count.index)}"
    min_master_nodes   = "${(var.master_node_count / 2) + 1}"
    region             = "${var.region}"
    security_groups    = "[${aws_security_group.master-node-sg.id}, ${aws_security_group.data-node-sg.id}]"
    availability_zones = "[${join(",", var.vpc_azs)}]"
    cluster_tag        = "${var.name_prefix}-elasticsearch-cluster"
  }
}

