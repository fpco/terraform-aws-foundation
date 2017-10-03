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
  aws_cloud    = "${var.is_govcloud ? "aws-us-gov" : "aws"}"
  source       = "../persistent-ebs-volumes"
  name_prefix  = "${var.name_prefix}-data-node"
  volume_count = "${var.data_node_count}"
  azs          = ["${data.aws_subnet.private.*.availability_zone}"]
  size         = "${var.data_node_ebs_size}"
  snapshot_ids = ["${var.data_node_snapshot_ids}"]
  encrypted    = "false"
  device_name  = "/dev/xvdf"
  max_wait     = 3600
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
  target_group_arns    = ["${concat(list(aws_alb_target_group.elasticsearch-api.arn), aws_alb_target_group.elasticsearch-api-secured.*.arn)}"]
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
    elasticsearch_version      = "${var.elasticsearch_version}"
    credstash_install_snippet  = "${var.credstash_install_snippet}"
    credstash_get_cmd          = "${var.credstash_get_cmd}"
    credstash_ca_cert_name     = "${var.name_prefix}-logstash-ca-cert"
    credstash_client_cert_name = "${var.name_prefix}-logstash-client-cert"
    credstash_client_key_name  = "${var.name_prefix}-logstash-client-key"
    credstash_context          = "env=${var.name_prefix}"
    is_master_node             = false
    logstash_beats_address     = "${var.logstash_beats_address}"
    extra_setup_snippet        = <<EXTRA_SETUP
${var.external_alb_setup ? data.template_file.proxy-setup.rendered : ""}

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


# TODO: move out from here or require only when external_alb_setup = true
data "aws_acm_certificate" "elasticsearch-cert" {
  domain = "${coalesce(var.elasticsearch_dns_ssl_name, var.elasticsearch_dns_name)}"
  statuses = ["ISSUED"]
}

resource "aws_alb_target_group" "elasticsearch-api" {
  name     = "${var.name_prefix}-ES-api"
  port     = 9200
  protocol = "HTTP"
  vpc_id   = "${var.vpc_id}"
  health_check = {
    interval = 60
  }
}

resource "aws_alb_listener" "elasticsearch-api" {
  load_balancer_arn = "${var.internal_alb["arn"]}"
  port              = 9200
  protocol          = "HTTP"

  default_action {
    target_group_arn = "${aws_alb_target_group.elasticsearch-api.arn}"
    type             = "forward"
  }
}

resource "aws_alb_listener_rule" "elasticsearch-api" {
  listener_arn = "${aws_alb_listener.elasticsearch-api.arn}"
  priority     = 99

  action {
    type = "forward"
    target_group_arn = "${aws_alb_target_group.elasticsearch-api.arn}"
  }

  condition {
    field  = "host-header"
    values = ["${var.elasticsearch_dns_name}"]
  }
}


// Optional ES API endpoint with BasicAuth
resource "aws_alb_target_group" "elasticsearch-api-secured" {
  count    = "${var.external_alb_setup ? 1 : 0}"
  name     = "${var.name_prefix}-ES-api-secured"
  port     = 9201
  protocol = "HTTP"
  vpc_id   = "${var.vpc_id}"
  health_check {
    interval = 300
    matcher  = "401"
  }
}

resource "aws_alb_listener" "elasticsearch-api-secured" {
  count             = "${var.external_alb_setup ? 1 : 0}"
  load_balancer_arn = "${lookup(var.external_alb, "arn", "")}"
  port              = 9201
  protocol          = "HTTPS"
  ssl_policy        = "ELBSecurityPolicy-2016-08"
  certificate_arn   = "${data.aws_acm_certificate.elasticsearch-cert.arn}"

  default_action {
    target_group_arn = "${element(aws_alb_target_group.elasticsearch-api-secured.*.arn, 0)}"
    type             = "forward"
  }
}

resource "aws_alb_listener_rule" "elasticsearch-api-secured" {
  count        = "${var.external_alb_setup ? 1 : 0}"
  listener_arn = "${element(aws_alb_listener.elasticsearch-api-secured.*.arn, 0)}"
  priority     = 99

  action {
    type = "forward"
    target_group_arn = "${element(aws_alb_target_group.elasticsearch-api-secured.*.arn, 0)}"
  }

  condition {
    field  = "host-header"
    values = ["${var.elasticsearch_dns_name}"]
  }
}
