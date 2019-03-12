output "elb_dns" {
  value       = "http://${aws_elb.web.dns_name}"
  description = "DNS name of the ELB"
}

output "elb_dns_instances" {
  value       = "${aws_elb.web.instances.dns_name}"
  description = "Number of Instancess in the ELB"
}
