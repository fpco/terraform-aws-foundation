output "elb_name" {
  value = "${aws_elb.logstash-elb.name}"
}

output "logstash_role_name" {
  value = "${aws_iam_role.logstash-role.name}"
}

output "asg_name" {
  value = "${aws_autoscaling_group.logstash-asg.name}"
}

output "logstash_dns" {
  value = {
    "dns_name"     = "${var.logstash_dns_name}"
    "elb_dns_name" = "${aws_elb.logstash-elb.dns_name}"
    "elb_zone_id"  = "${aws_elb.logstash-elb.zone_id}"
  }
}
