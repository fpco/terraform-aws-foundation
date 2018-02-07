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
  value = "${aws_autoscaling_group.kibana-asg.name}"
}

output "http_target_group_arn" {
  value = "${aws_alb_target_group.kibana-http.arn}"
}

output "https_target_group_arn" {
  value = "${aws_alb_target_group.kibana-https.arn}"
}
