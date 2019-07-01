output "security_group_id" {
  value = aws_security_group.kibana-sg.id
}

output "setup_snippet" {
  value       = data.template_file.kibana-setup.rendered
  description = "This snippet can be used to install and configure Kibana alongside anyother services"
}

output "asg_name" {
  value = aws_autoscaling_group.kibana-asg.*.name
}

output "http_target_group_arn" {
  value = element(
    coalescelist(aws_alb_target_group.kibana-http.*.arn, [""]),
    0,
  )
}

output "https_target_group_arn" {
  value = element(
    coalescelist(aws_alb_target_group.kibana-https.*.arn, [""]),
    0,
  )
}

output "lb" {
  value = {
    "dns_name" = element(
      coalescelist(
        aws_elb.kibana-elb.*.dns_name,
        [lookup(var.alb, "dns_name", "")],
      ),
      0,
    )
    "zone_id" = element(
      coalescelist(
        aws_elb.kibana-elb.*.zone_id,
        [lookup(var.alb, "zone_id", "")],
      ),
      0,
    )
    "security_group_id" = var.alb["security_group_id"]
  }

  description = "ELB related info. Will either be ELB or ALB info."
}

output "elb_name" {
  value = aws_elb.kibana-elb.*.name
}

