output "elb_dns" {
  value       = "http://${aws_elb.web.dns_name}"
  description = "make the ELB accessible on the outside"
}

output "elb_dns_instances" {
  value       = "${aws_elb.web.instances}"
  description = "make the ELB accessible on the outside"
}
