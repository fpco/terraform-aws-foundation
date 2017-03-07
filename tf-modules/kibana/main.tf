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
  domain = "${var.kibana_dns_name}"
  statuses = ["ISSUED"]
}

resource "aws_elb" "kibana-elb" {
  name = "${var.name_prefix}-kibana-elb"
  subnets       = ["${var.subnet_ids}"]
  security_groups = ["${aws_security_group.kibana-elb-sg.id}"]
  
  listener {
    instance_port = 5601
    instance_protocol = "http"
    lb_port = 443
    lb_protocol = "https"
    ssl_certificate_id = "${data.aws_acm_certificate.kibana-cert.arn}"
  }

  health_check {
    healthy_threshold = 2
    unhealthy_threshold = 2
    timeout = 3
    target = "HTTP:5601/"
    interval = 30
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
    elasticsearch_url = "${var.elasticsearch_url}"
  }
}


resource "aws_security_group" "kibana-sg" {
  name        = "${var.name_prefix}-kibana-sg"
  vpc_id      = "${var.vpc_id}"
  description = "Allow ICMP, Kibana default port (5601) and everything outbound."

  ingress {
    from_port   = 5601
    to_port     = 5601
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


resource "aws_security_group" "kibana-elb-sg" {
  name        = "${var.name_prefix}-kibana-elb-sg"
  vpc_id      = "${var.vpc_id}"
  description = "Allow ICMP, HTTPS and everything outbound."

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 443
    to_port     = 443
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


resource "aws_autoscaling_group" "kibana-asg" {
  count                = "${min(var.max_server_count, 1)}"
  availability_zones   = ["${var.vpc_azs}"]
  name                 = "${var.name_prefix}-kibana-asg"
  max_size             = "${var.max_server_count}"
  min_size             = "${var.min_server_count}"
  desired_capacity     = "${var.desired_server_count}"
  launch_configuration = "${aws_launch_configuration.kibana-lc.name}"
  health_check_type    = "ELB"
  vpc_zone_identifier  = ["${var.subnet_ids}"]
  load_balancers       = ["${aws_elb.kibana-elb.name}"]

  tag = [{
    key                 = "Name"
    value               = "${var.name_prefix}-kibana"
    propagate_at_launch = true
  }]

}


resource "aws_launch_configuration" "kibana-lc" {
  count           = "${min(var.max_server_count, 1)}"
  name_prefix     = "${var.name_prefix}-kibana-lc-"
  image_id        = "${var.ami}"
  instance_type   = "${var.instance_type}"
  key_name        = "${var.key_name}"
  security_groups = ["${aws_security_group.kibana-sg.id}"]
  user_data       = <<USER_DATA
#!/bin/bash
${data.template_file.kibana-setup.rendered}
USER_DATA

  associate_public_ip_address = true

  lifecycle = {
    create_before_destroy = true
  }
}
