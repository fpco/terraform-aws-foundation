//Export the `key_file` variable for convenience
output "key_file" {
    value = "${var.key_file}"
}
//`name` exported from the Server `aws_autoscaling_group`
output "asg_name" {
    value = "${module.server.name}"
}
