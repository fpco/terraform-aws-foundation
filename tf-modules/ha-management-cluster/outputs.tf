# module outputs!
output "services_security_group" {
    value = "${aws_security_group.management_services.id}"
}
output "asg_name" {
    value = "${module.management-cluster.asg_name}"
}
