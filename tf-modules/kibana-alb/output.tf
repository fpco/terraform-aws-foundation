// ALB related info.
output "alb" {
  value = {
    "arn"               = "${aws_alb.kibana.arn}"
    "dns_name"          = "${aws_alb.kibana.dns_name}"
    "zone_id"           = "${aws_alb.kibana.zone_id}"
    "security_group_id" = "${aws_security_group.alb-sg.id}"
  }
}
