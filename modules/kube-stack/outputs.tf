output "kube_controllers_dns" {
  description = "CNAME DNS record that points to the AWS ELB for the kube controllers"
  value       = aws_elb.kube-controllers.dns_name
}

