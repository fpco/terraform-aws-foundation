output "elb_dns" {
  value       = "http://${aws_elb.web.dns_name}"
  description = "DNS name of the ELB"
}

output "elb_name" {
  value       = aws_elb.web.name
  description = "ELB Name"
}

output "elb_instances" {
  value       = aws_elb.web.instances
  description = "ELB Instances"
}

output "elb_arn" {
  value       = aws_elb.web.arn
  description = "ELB Arn"
}

