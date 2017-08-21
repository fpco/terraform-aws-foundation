output "elb_dns" {
  value = "${aws_elb.kibana-elb.dns_name}"
}

output "elb_name" {
  value = "${aws_elb.kibana-elb.name}"
}

output "security_group_id" {
  value = "${aws_security_group.kibana-sg.id}"
}

output "setup_snippet" {
  value = "${data.template_file.kibana-setup.rendered}"
}

output "asg_name" {
  value = "${aws_autoscaling_group.kibana-asg.name}"
}
