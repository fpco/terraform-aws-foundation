output "key_file" {
    value = "${var.key_file}"
}
output "asg_name" {
    value = "${module.prometheus-server.name}"
}
