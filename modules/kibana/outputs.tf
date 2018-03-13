output "security_group_id" {
  value = "${aws_security_group.kibana-sg.id}"
}

// This snippet can be used to install and configure Kibana
// alongside anyother services, for instance deploying it
// on the same instance together with Logstash
output "setup_snippet" {
  value = "${data.template_file.kibana-setup.rendered}"
}

output "asg_name" {
  value = "${aws_autoscaling_group.kibana-asg.*.name}"
}

output "http_target_group_arn" {
  value = "${element(coalescelist(aws_alb_target_group.kibana-http.*.arn, list("")), 0)}"
}

output "https_target_group_arn" {
  value = "${element(coalescelist(aws_alb_target_group.kibana-https.*.arn, list("")), 0)}"
}


//  ELB related info. Will either be ELB or ALB info.
output "lb" {
  value = {
    "dns_name"          = "${element(coalescelist(aws_elb.kibana-elb.*.dns_name, list(lookup(var.alb, "dns_name", ""))), 0)}"
    "zone_id"           = "${element(coalescelist(aws_elb.kibana-elb.*.zone_id, list(lookup(var.alb, "zone_id", ""))), 0)}"
    "security_group_id" = "${var.alb["security_group_id"]}"
  }
}

output "elb_name" {
  value = "${aws_elb.kibana-elb.*.name}"
}
