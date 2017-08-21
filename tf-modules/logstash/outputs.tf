output "elb_dns" {
  value = "${aws_elb.logstash-elb.dns_name}"
}

output "elb_name" {
  value = "${aws_elb.logstash-elb.name}"
}

output "logstash_role_name" {
  value = "${aws_iam_role.logstash-role.name}"
}

output "asg_name" {
  value = "${aws_autoscaling_group.logstash-asg.name}"
}
