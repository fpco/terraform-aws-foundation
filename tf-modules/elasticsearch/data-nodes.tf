/**
 *##Deployment configuration for data nodes for Elasticsearh cluster
 *
 * Just as with master nodes, each data node has a persistent EBS volume created for.
 * Data nodes also function as coordinators, thus each one has 9200 port open for the API.
 * Elastic load balancer is created for elasticsearch API enpoint. Every node is managed
 * by an auto scaling group, so if a helath check fails for an instance it will be
 * torn down and a new one deployed with the same EBS volume mounted.
 */

module "data-node-ebs-volumes" {
  source       = "../persistent-ebs-volumes"
  name_prefix  = "${var.name_prefix}-data-node"
  volume_count = "${var.data_node_count}"
  azs          = ["${data.aws_subnet.private.*.availability_zone}"]
  size         = "${var.data_node_ebs_size}"
  snapshot_ids = ["${var.data_node_snapshot_ids}"]
  encrypted    = "false"
  device_name  = "/dev/xvdf"
  extra_tags   = {
    cluster = "${var.name_prefix}-elasticsearch-cluster"
  }
}

resource "aws_iam_role_policy_attachment" "data-node-attach-ebs-volume" {
  count = "${var.data_node_count}"
  role = "${element(aws_iam_role.data-node-role.*.name, count.index)}"
  policy_arn = "${element(module.data-node-ebs-volumes.iam_volume_policy_arns, count.index)}"
}

resource "aws_launch_configuration" "data-node-lc" {
  count                = "${var.data_node_count}"
  name_prefix          = "${var.name_prefix}-data-node-${format("%02d", count.index)}-${element(data.aws_subnet.private.*.availability_zone, count.index)}-"
  image_id             = "${var.node_ami}"
  instance_type        = "${var.data_node_instance_type}"
  iam_instance_profile = "${element(aws_iam_instance_profile.data-node-iam-profile.*.id, count.index)}"
  key_name             = "${var.key_name}"
  security_groups      = ["${concat(list(aws_security_group.transport-sg.id, aws_security_group.elasticsearch-api-sg.id), var.extra_sg_ids)}"]
  user_data            = "${element(data.template_file.data-node-setup.*.rendered, count.index)}"
  lifecycle            = {
    create_before_destroy = true
  }
}


# A single data node autoscaling group.
resource "aws_autoscaling_group" "data-node-asg" {
  count                = "${var.data_node_count}"
  availability_zones   = ["${element(data.aws_subnet.private.*.availability_zone, count.index)}"]
  name                 = "${var.name_prefix}-data-node-${format("%02d", count.index)}-${element(data.aws_subnet.private.*.availability_zone, count.index)}"
  max_size             = 1
  min_size             = 1
  desired_capacity     = 1
  launch_configuration = "${element(aws_launch_configuration.data-node-lc.*.name, count.index)}"
  health_check_type    = "ELB"
  vpc_zone_identifier  = ["${element(var.private_subnet_ids, count.index)}"]
  load_balancers       = ["${aws_elb.elasticsearch-elb.name}"]
  lifecycle            = {
    create_before_destroy = true
  }

  tag = [{
    key                 = "Name"
    value               = "${var.name_prefix}-data-node-${format("%02d", count.index)}-${element(data.aws_subnet.private.*.availability_zone, count.index)}"
    propagate_at_launch = true
  },{
    key                 = "cluster"
    value               = "${var.name_prefix}-elasticsearch-cluster"
    propagate_at_launch = true
  }]
}

data "template_file" "data-node-setup" {
  count    = "${var.data_node_count}"
  template = "${file("${path.module}/data/setup.tpl.sh")}"

  vars {
    mount_snippet              = "${element(module.data-node-ebs-volumes.volume_mount_snippets, count.index)}"
    device_name                = "/dev/xvdf"
    mount_point                = "/mnt/elasticsearch"
    wait_interval              = 1
    node_name                  = "${var.name_prefix}-data-node-${format("%02d", count.index)}-${element(data.aws_subnet.private.*.availability_zone, count.index)}"
    config_yaml                = "${element(data.template_file.data-node-config.*.rendered, count.index)}"
    credstash_install_snippet  = "${var.credstash_install_snippet}"
    credstash_get_cmd          = "${var.credstash_get_cmd}"
    credstash_ca_cert_name     = "${var.name_prefix}-logstash-ca-cert"
    credstash_client_cert_name = "${var.name_prefix}-logstash-client-cert"
    credstash_client_key_name  = "${var.name_prefix}-logstash-client-key"
    credstash_context          = "env=${var.name_prefix}"
    is_master_node             = false
    logstash_beats_address     = "${var.logstash_beats_address}"
    extra_setup_snippet        = <<EXTRA_SETUP
${var.deploy_proxy ? data.template_file.proxy-setup.rendered : ""}

${var.extra_setup_snippet}
EXTRA_SETUP
  }
}

data "template_file" "proxy-setup" {
  template = "${file("${path.module}/data/nginx-setup.tpl.sh")}"

  vars {
    credstash_install_snippet = "${var.credstash_install_snippet}"
    credstash_get_cmd         = "${var.credstash_get_cmd}"
    credstash_context         = "env=${var.name_prefix}"
    nginx_username_key        = "${var.name_prefix}-elasticsearch-basic-auth-username"
    nginx_password_key        = "${var.name_prefix}-elasticsearch-basic-auth-password"
  }
}


data "template_file" "data-node-config" {
  count    = "${var.data_node_count}"
  template = "${file("${path.module}/data/config.tpl.yml")}"

  vars {
    is_master          = false
    node_name          = "${var.name_prefix}-data-node-${format("%02d", count.index)}-${element(data.aws_subnet.private.*.availability_zone, count.index)}"
    min_master_nodes   = "${(var.master_node_count / 2) + 1}"
    security_groups    = "[${aws_security_group.transport-sg.id}, ${aws_security_group.elasticsearch-api-sg.id}]"
    availability_zones = "[${join(",", data.aws_subnet.private.*.availability_zone)}]"
    cluster_tag        = "${var.name_prefix}-elasticsearch-cluster"
    region             = "${data.aws_region.current.name}"
    extra_config       = "${var.extra_config}"
  }
}



data "aws_acm_certificate" "elasticsearch-cert" {
  domain = "${coalesce(var.elasticsearch_dns_ssl_name, var.elasticsearch_dns_name)}"
  statuses = ["ISSUED"]
}

resource "aws_elb" "elasticsearch-elb" {
  name            = "${var.name_prefix}-elasticsearch"
  subnets         = ["${var.public_subnet_ids}"]
  security_groups = ["${concat(list(aws_security_group.elasticsearch-elb-sg.id), var.extra_elb_sg_ids)}"]
  internal        = "${var.internal}"

  listener {
    instance_port = 9200
    instance_protocol = "http"
    lb_port = 9200
    lb_protocol = "http"
  }

  listener {
    instance_port = 9201
    instance_protocol = "http"
    lb_port = 9201
    lb_protocol = "https"
    ssl_certificate_id = "${data.aws_acm_certificate.elasticsearch-cert.arn}"
  }

  health_check {
    healthy_threshold = 2
    unhealthy_threshold = 3
    timeout = 3
    target = "HTTP:9200/"
    interval = 60
  }

  cross_zone_load_balancing = true
  idle_timeout              = 30
  connection_draining       = false

}
