output "logstash_role_name" {
  value = "${aws_iam_role.logstash-role.name}"
}

output "asg_name" {
  value = "${aws_autoscaling_group.logstash-asg.name}"
}

output "elb_name" {
  value = "${aws_elb.logstash-elb.name}"
}

output "elb" {
  value = {
    "dns_name"          = "${aws_elb.logstash-elb.dns_name}"
    "zone_id"           = "${aws_elb.logstash-elb.zone_id}"
    "security_group_id" = "${aws_security_group.logstash-elb-sg.id}"
  }
}
