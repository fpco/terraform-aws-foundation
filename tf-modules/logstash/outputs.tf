output "elb_dns" {
  value = "${aws_elb.logstash-elb.dns_name}"
}

output "elb_name" {
  value = "${aws_elb.logstash-elb.name}"
}

output "security_group_id" {
  value = "${aws_security_group.logstash-sg.id}"
}

output "setup_snippet" {
  value = "${data.template_file.logstash-setup.rendered}"
}
