output "elb_dns" {
  value       = "http://${aws_elb.web.dns_name}"
  description = "DNS name of the ELB"
}
