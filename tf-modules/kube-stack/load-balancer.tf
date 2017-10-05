
resource "aws_elb" "kube-controllers" {
  name            = "${var.name_prefix}-kube-api"
  subnets         = ["${var.lb_subnet_ids}"]
  security_groups = ["${var.lb_security_group_ids}"]
  internal        = "${var.private_load_balancer}"

  #listener {
  #  instance_port = 6443
  #  instance_protocol = "https"
  #  lb_port = 8080
  #  lb_protocol = "http"
  #  #ssl_certificate_id = "${data.aws_acm_certificate.kube-cert.arn}"
  #}

  listener {
    instance_port = 6443
    instance_protocol = "tcp"
    lb_port = 443
    lb_protocol = "tcp"
  }

  health_check {
    healthy_threshold = 2
    unhealthy_threshold = 3
    timeout = 5
    target = "TCP:6443"
    interval = 20
  }

  cross_zone_load_balancing = false
  idle_timeout = 60
  connection_draining = true
  connection_draining_timeout = 60

  tags {
    Name = "${var.name_prefix}-kube-api"
  }
}

