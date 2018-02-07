// ALB related info.
output "alb" {
  value = {
    "arn"               = "${aws_alb.web-server.arn}"
    "dns_name"          = "${aws_alb.web-server.dns_name}"
    "zone_id"           = "${aws_alb.web-server.zone_id}"
    "security_group_id" = "${aws_security_group.alb-sg.id}"
  }
}
