/**
 *## Kibana
 *
 * This module takes care of deployment of EC2 instances running Kibana using
 * an autoscaling group with a load balancer. It also adds an entry to Route53
 * for the Kibana load balancer. ISSUED SSL certificate must exist in ACM for
 * specified the `kibana_dns`.
 *
 */
data "aws_acm_certificate" "kibana-cert" {
  domain = "${coalesce(var.kibana_dns_ssl_name, var.kibana_dns_name)}"
  statuses = ["ISSUED"]
}

data "aws_subnet" "private" {
  count  = "${length(var.public_subnet_ids)}"
  id     = "${var.public_subnet_ids[count.index]}"
  vpc_id = "${var.vpc_id}"
}

resource "aws_elb" "kibana-elb" {
  name            = "${var.name_prefix}-kibana"
  subnets         = ["${var.public_subnet_ids}"]
  security_groups = ["${concat(list(aws_security_group.kibana-elb-sg.id), var.extra_elb_sg_ids)}"]
  internal        = "${var.internal}"

  listener {
    instance_port = 5602
    instance_protocol = "http"
    lb_port = 443
    lb_protocol = "https"
    ssl_certificate_id = "${data.aws_acm_certificate.kibana-cert.arn}"
  }

  listener {
    instance_port = 80
    instance_protocol = "http"
    lb_port = 80
    lb_protocol = "http"
  }

  health_check {
    healthy_threshold = 2
    unhealthy_threshold = 5
    timeout = 10
    target = "HTTP:5603/"
    interval = 60
  }

  cross_zone_load_balancing = true
  idle_timeout = 60
  connection_draining = true
  connection_draining_timeout = 60

  tags {
    Name = "${var.name_prefix}-kibana-elb"
  }
}


resource "aws_route53_record" "kibana-elb" {
  zone_id = "${var.route53_zone_id}"
  name = "${var.kibana_dns_name}"
  type = "A"

  alias {
    name = "${aws_elb.kibana-elb.dns_name}"
    zone_id = "${aws_elb.kibana-elb.zone_id}"
    evaluate_target_health = true
  }
}


data "template_file" "kibana-setup" {
  template = "${file("${path.module}/data/setup.tpl.sh")}"

  vars {
    elasticsearch_url         = "${var.elasticsearch_url}"
    credstash_install_snippet = "${var.credstash_install_snippet}"
    credstash_get_cmd         = "${var.credstash_get_cmd}"
    credstash_context         = "env=${var.name_prefix}"
    nginx_username_key        = "${var.name_prefix}-kibana-basic-auth-username"
    nginx_password_key        = "${var.name_prefix}-kibana-basic-auth-password"
  }
}


resource "aws_security_group" "kibana-sg" {
  name        = "${var.name_prefix}-kibana-instance"
  vpc_id      = "${var.vpc_id}"
  description = "Allow inboud HTTP, Kibana port (5602), Kibana healthcheck (5603) from ELB. Also everything outbound."

  tags {
    Name = "${var.name_prefix}-kibana-nodes"
  }

  # Kibana
  ingress {
    from_port       = 5602
    to_port         = 5602
    protocol        = "tcp"
    security_groups = ["${aws_security_group.kibana-elb-sg.id}"]
  }

  # Kibana status
  ingress {
    from_port       = 5603
    to_port         = 5603
    protocol        = "tcp"
    security_groups = ["${aws_security_group.kibana-elb-sg.id}"]
  }

  # Used for proper redirect to https
  ingress {
    from_port       = 80
    to_port         = 80
    protocol        = "tcp"
    security_groups = ["${aws_security_group.kibana-elb-sg.id}"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}


resource "aws_security_group" "kibana-elb-sg" {
  name        = "${var.name_prefix}-kibana-elb"
  vpc_id      = "${var.vpc_id}"
  description = "Allow HTTP (80), HTTPS (443) and everything outbound."

  tags {
    Name = "${var.kibana_dns_name}"
  }

  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["${var.elb_ingress_cidrs}"]
  }

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["${var.elb_ingress_cidrs}"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}


resource "aws_autoscaling_group" "kibana-asg" {
  count                = "${min(var.max_server_count, 1)}"
  availability_zones   = ["${data.aws_subnet.private.*.availability_zone}"]
  name                 = "${var.name_prefix}-kibana"
  max_size             = "${var.max_server_count}"
  min_size             = "${var.min_server_count}"
  desired_capacity     = "${var.desired_server_count}"
  launch_configuration = "${aws_launch_configuration.kibana-lc.name}"
  health_check_type    = "ELB"
  vpc_zone_identifier  = ["${var.private_subnet_ids}"]
  load_balancers       = ["${aws_elb.kibana-elb.name}"]

  tag = [{
    key                 = "Name"
    value               = "${var.name_prefix}-kibana"
    propagate_at_launch = true
  }]

}


resource "aws_launch_configuration" "kibana-lc" {
  count           = "${min(var.max_server_count, 1)}"
  name_prefix     = "${var.name_prefix}-kibana-"
  image_id        = "${var.ami}"
  instance_type   = "${var.instance_type}"
  kibana_version  = "${var.kibana_version}"
  key_name        = "${var.key_name}"
  security_groups = "${concat(list(aws_security_group.kibana-sg.id), var.extra_sg_ids)}"
  user_data       = <<USER_DATA
#!/bin/bash
${data.template_file.kibana-setup.rendered}
USER_DATA

  lifecycle = {
    create_before_destroy = true
  }
}
