data "aws_acm_certificate" "web-server-cert" {
  domain   = "${coalesce(var.dns_ssl_name, var.dns_name)}"
  statuses = ["ISSUED"]
}

resource "aws_security_group" "alb-sg" {
  name        = "${var.name_prefix}-alb"
  vpc_id      = "${var.vpc_id}"
  description = "Security group for ${var.app_names} ALB."

  tags {
    Name = "${var.name_prefix}-alb"
  }
}

// Allow all egress traffic
resource "aws_security_group_rule" "alb-egress-rule" {
  type              = "egress"
  from_port         = 0
  to_port           = 0
  protocol          = "-1"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.alb-sg.id}"
}

// Rule that allows ingress to ALB on port 80 from custom CIDRs,
// usually for redirect of HTTP to HTTPS
resource "aws_security_group_rule" "alb-http-rule" {
  count             = "${length(var.user_ingress_cidrs) > 0 ? 1 : 0}"
  type              = "ingress"
  from_port         = 80
  to_port           = 80
  protocol          = "tcp"
  cidr_blocks       = ["${var.user_ingress_cidrs}"]
  security_group_id = "${aws_security_group.alb-sg.id}"
}

// Rule that allows ingress to ALB on port 443 from custom CIDRs
resource "aws_security_group_rule" "alb-https-rule" {
  count             = "${length(var.user_ingress_cidrs) > 0 ? 1 : 0}"
  type              = "ingress"
  from_port         = 443
  to_port           = 443
  protocol          = "tcp"
  cidr_blocks       = ["${var.user_ingress_cidrs}"]
  security_group_id = "${aws_security_group.alb-sg.id}"
}

// Application Load Balancer for Web Server.
resource "aws_alb" "web-server" {
  name            = "${var.name_prefix}-alb"
  internal        = "${var.internal}"
  idle_timeout    = "300"
  security_groups = ["${aws_security_group.alb-sg.id}"]
  subnets         = ["${var.subnet_ids}"]

  tags {
    Name = "${var.name_prefix}-${var.app_names}-alb"
    Apps = "${var.app_names}"
  }
}

resource "aws_alb_listener" "http" {
  load_balancer_arn = "${aws_alb.web-server.arn}"
  port              = "80"
  protocol          = "HTTP"

  default_action {
    type             = "forward"
    target_group_arn = "${var.http_target_group_arn}"
  }
}

resource "aws_alb_listener_rule" "http" {
  listener_arn = "${aws_alb_listener.http.arn}"
  priority     = 99

  action {
    type             = "forward"
    target_group_arn = "${var.http_target_group_arn}"
  }

  condition {
    field  = "host-header"
    values = ["${var.dns_name}"]
  }
}

resource "aws_alb_listener" "https" {
  load_balancer_arn = "${aws_alb.web-server.arn}"
  port              = "443"
  protocol          = "HTTPS"
  ssl_policy        = "ELBSecurityPolicy-2016-08"
  certificate_arn   = "${data.aws_acm_certificate.web-server-cert.arn}"

  default_action {
    type             = "forward"
    target_group_arn = "${var.https_target_group_arn}"
  }
}

resource "aws_alb_listener_rule" "https" {
  listener_arn = "${aws_alb_listener.https.arn}"
  priority     = 99

  action {
    type             = "forward"
    target_group_arn = "${var.https_target_group_arn}"
  }

  condition {
    field  = "host-header"
    values = ["${var.dns_name}"]
  }
}
