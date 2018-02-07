/**
 * ## Logstash
 *
 * This module takes care of deployment of EC2 instances running Logstash using
 * an autoscaling group with a load balancer. It also adds an entry to Route53
 * for the Logstash load balancer.
 *
 */

resource "aws_elb" "logstash-elb" {
  name            = "${var.name_prefix}-logstash"
  subnets         = ["${var.public_subnet_ids}"]
  security_groups = ["${aws_security_group.logstash-elb-sg.id}"]
  internal        = "${var.internal}"

  listener {
    instance_port     = 5044
    instance_protocol = "tcp"
    lb_port           = 5044
    lb_protocol       = "tcp"
  }

  health_check {
    healthy_threshold   = 2
    unhealthy_threshold = 5
    timeout             = 10
    target              = "HTTP:8080/"
    interval            = 60
  }

  cross_zone_load_balancing   = true
  idle_timeout                = 60
  connection_draining         = true
  connection_draining_timeout = 60

  tags {
    Name = "${var.name_prefix}-logstash-elb"
  }
}

data "template_file" "logstash-setup" {
  template = "${file("${path.module}/data/setup.tpl.sh")}"

  vars {
    credstash_install_snippet     = "${var.credstash_install_snippet}"
    credstash_get_cmd             = "${var.credstash_get_cmd}"
    credstash_ca_cert_name        = "${var.name_prefix}-logstash-ca-cert"
    credstash_server_cert_name    = "${var.name_prefix}-logstash-server-cert"
    credstash_server_key_name     = "${var.name_prefix}-logstash-server-key"
    credstash_dynamic_config_name = "${var.name_prefix}-logstash-dynamic-conf"
    credstash_context             = "env=${var.name_prefix}"
    name_suffix                   = "${var.name_suffix}"
    logstash_version              = "${var.logstash_version}"
    config                        = "${data.template_file.logstash-config.rendered}"
    extra_config                  = "${var.extra_config}"
    extra_settings                = "${var.extra_settings}"
    extra_grok_patterns           = "${var.extra_grok_patterns}"
    extra_setup_snippet           = "${var.extra_setup_snippet}"
  }
}

data "template_file" "logstash-config" {
  template = "${file("${path.module}/data/config.tpl.conf")}"

  vars {
    elasticsearch_url = "${var.elasticsearch_url}"
    name_suffix       = "${var.name_suffix}"
  }
}

data "aws_subnet" "public" {
  count  = "${length(var.public_subnet_ids)}"
  id     = "${var.public_subnet_ids[count.index]}"
  vpc_id = "${var.vpc_id}"
}

data "aws_subnet" "private" {
  count  = "${length(var.private_subnet_ids)}"
  id     = "${var.private_subnet_ids[count.index]}"
  vpc_id = "${var.vpc_id}"
}

resource "aws_security_group" "logstash-sg" {
  name        = "${var.name_prefix}-logstash-instance"
  vpc_id      = "${var.vpc_id}"
  description = "Allow Logstash Beat port (5044), Logstash HTTP health check (8080) from ELB. Also everything outbound."

  tags {
    Name = "${var.name_prefix}-logstash-nodes"
  }

  ingress {
    from_port       = 5044
    to_port         = 5044
    protocol        = "tcp"
    security_groups = ["${aws_security_group.logstash-elb-sg.id}"]
  }

  ingress {
    from_port       = 8080
    to_port         = 8080
    protocol        = "tcp"
    security_groups = ["${aws_security_group.logstash-elb-sg.id}"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "logstash-elb-sg" {
  name        = "${var.name_prefix}-logstash-elb"
  vpc_id      = "${var.vpc_id}"
  description = "Allow Logstash Beat port (5044) and everything outbound."

  tags {
    Name = "${var.logstash_dns_name}"
  }

  ingress {
    from_port   = 5044
    to_port     = 5044
    protocol    = "tcp"
    cidr_blocks = ["${distinct(concat(data.aws_subnet.public.*.cidr_block, data.aws_subnet.private.*.cidr_block, var.extra_elb_ingress_cidrs))}"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_autoscaling_group" "logstash-asg" {
  count                     = "${min(var.max_server_count, 1)}"
  availability_zones        = ["${data.aws_subnet.private.*.availability_zone}"]
  vpc_zone_identifier       = ["${var.private_subnet_ids}"]
  name                      = "${var.name_prefix}-logstash${var.name_suffix}"
  max_size                  = "${var.max_server_count}"
  min_size                  = "${var.min_server_count}"
  desired_capacity          = "${var.desired_server_count}"
  launch_configuration      = "${aws_launch_configuration.logstash-lc.name}"
  health_check_type         = "ELB"
  health_check_grace_period = "600"
  load_balancers            = ["${aws_elb.logstash-elb.name}"]
  target_group_arns         = ["${var.target_group_arns}"]

  tag = [{
    key                 = "Name"
    value               = "${var.name_prefix}-logstash${var.name_suffix}"
    propagate_at_launch = true
  }]
}

resource "aws_launch_configuration" "logstash-lc" {
  count                = "${min(var.max_server_count, 1)}"
  name_prefix          = "${var.name_prefix}-logstash${var.name_suffix}-"
  image_id             = "${var.ami}"
  instance_type        = "${var.instance_type}"
  key_name             = "${var.key_name}"
  security_groups      = ["${concat(list(aws_security_group.logstash-sg.id), var.extra_sg_ids)}"]
  iam_instance_profile = "${aws_iam_instance_profile.logstash-profile.id}"
  user_data            = "${data.template_file.logstash-setup.rendered}"

  lifecycle = {
    create_before_destroy = true
  }
}
