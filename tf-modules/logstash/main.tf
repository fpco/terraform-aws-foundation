/**
 *## Logstash
 *
 * This module takes care of deployment of EC2 instances running Logstash using
 * an autoscaling group with a load balancer. It also adds an entry to Route53
 * for the Logstash load balancer.
 *
 */

resource "aws_elb" "logstash-elb" {
  name            = "${var.name_prefix}-logstash-elb"
  subnets         = ["${var.subnet_ids}"]
  security_groups = ["${aws_security_group.logstash-elb-sg.id}"]

  listener {
    instance_port     = 5044
    instance_protocol = "tcp"
    lb_port           = 5044
    lb_protocol       = "tcp"
  }

  health_check {
    healthy_threshold   = 2
    unhealthy_threshold = 2
    timeout             = 3
    target              = "TCP:5044"
    interval            = 30
  }

  cross_zone_load_balancing   = true
  idle_timeout                = 60
  connection_draining         = true
  connection_draining_timeout = 60

  tags {
    Name = "${var.name_prefix}-logstash-elb"
  }

}


resource "aws_route53_record" "logstash-elb" {
  zone_id = "${var.route53_zone_id}"
  name    = "${var.logstash_dns_name}"
  type    = "A"

  alias {
    name                   = "${aws_elb.logstash-elb.dns_name}"
    zone_id                = "${aws_elb.logstash-elb.zone_id}"
    evaluate_target_health = true
  }
}


data "template_file" "logstash-setup" {
  template = "${file("${path.module}/data/setup.tpl.sh")}"

  vars {
    credstash_install_snippet     = "${module.credstash-reader.install_snippet}"
    credstash_get_cmd             = "${module.credstash-reader.get_cmd}"
    credstash_ca_cert_name        = "${var.credstash_prefix}${var.credstash_ca_cert_name}"
    credstash_server_cert_name    = "${var.credstash_prefix}${var.credstash_server_cert_name}"
    credstash_server_key_name     = "${var.credstash_prefix}${var.credstash_server_key_name}"
    credstash_dynamic_config_name = "${var.credstash_prefix}${var.credstash_dynamic_config_name}"
    credstash_dynamic_config_cron = "${var.credstash_dynamic_config_poll_schedule}"
    config                        = "${data.template_file.logstash-config.rendered}"
    extra_settings                = "${var.extra_settings}"
    extra_setup_snippet           = "${var.extra_setup_snippet}"
  }
}

data "template_file" "logstash-config" {
  template = "${file("${path.module}/data/config.tpl.conf")}"

  vars {
    elasticsearch_url = "${var.elasticsearch_url}"
  }
}


resource "aws_security_group" "logstash-sg" {
  name        = "${var.name_prefix}-logstash-sg"
  vpc_id      = "${var.vpc_id}"
  description = "Allow ICMP, SSH, Logstash Beat port (5044) and everything outbound."

  ingress {
    from_port   = 5044
    to_port     = 5044
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = -1
    to_port     = -1
    protocol    = "icmp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}


resource "aws_security_group" "logstash-elb-sg" {
  name        = "${var.name_prefix}-logstash-elb-sg"
  vpc_id      = "${var.vpc_id}"
  description = "Allow ICMP, TCP (5044) and everything outbound."

  ingress {
    from_port   = 5044
    to_port     = 5044
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = -1
    to_port     = -1
    protocol    = "icmp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}


resource "aws_autoscaling_group" "logstash-asg" {
  count                = "${min(var.max_server_count, 1)}"
  availability_zones   = ["${var.vpc_azs}"]
  vpc_zone_identifier  = ["${var.subnet_ids}"]
  name                 = "${var.name_prefix}-logstash-asg"
  max_size             = "${var.max_server_count}"
  min_size             = "${var.min_server_count}"
  desired_capacity     = "${var.desired_server_count}"
  launch_configuration = "${aws_launch_configuration.logstash-lc.name}"
  health_check_type    = "ELB"
  load_balancers       = ["${concat(list(aws_elb.logstash-elb.name), var.extra_elbs)}"]

  tag = [{
    key                 = "Name"
    value               = "${var.name_prefix}-logstash"
    propagate_at_launch = true
  }]

}


resource "aws_launch_configuration" "logstash-lc" {
  count                = "${min(var.max_server_count, 1)}"
  name_prefix          = "${var.name_prefix}-logstash-lc-"
  image_id             = "${var.ami}"
  instance_type        = "${var.instance_type}"
  key_name             = "${var.key_name}"
  security_groups      = ["${concat(list(aws_security_group.logstash-sg.id), var.extra_security_groups)}"]
  iam_instance_profile = "${aws_iam_instance_profile.logstash-profile.id}"
  user_data            = <<USER_DATA
#!/bin/bash
${data.template_file.logstash-setup.rendered}
${var.extra_setup_snippet}
USER_DATA

  associate_public_ip_address = true

  lifecycle = {
    create_before_destroy = true
  }

}


