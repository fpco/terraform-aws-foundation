output "kube_controllers_dns" {
  value       = "${aws_elb.kube-controllers.dns_name}"
  description = "CNAME DNS record that points to the AWS ELB for the kube controllers"
}
