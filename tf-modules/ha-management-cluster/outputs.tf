# module outputs!
output "services_security_group" {
    value = "${aws_security_group.management_services.id}"
}
output "user_data" {
    value = "${template_file.manage_init.rendered}"
}
