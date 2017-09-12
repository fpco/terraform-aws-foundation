output "security_group_id" {
  value = "${aws_security_group.kibana-sg.id}"
}

output "setup_snippet" {
  value = "${data.template_file.kibana-setup.rendered}"
}

output "asg_name" {
  value = "${aws_autoscaling_group.kibana-asg.name}"
}

output "elb_name" {
  value = "${aws_elb.kibana-elb.name}"
}
output "kibana_dns" {
  value = {
    "dns_name"     = "${var.kibana_dns_name}"
    "elb_dns_name" = "${aws_elb.kibana-elb.dns_name}"
    "elb_zone_id"  = "${aws_elb.kibana-elb.zone_id}"
  }
}
