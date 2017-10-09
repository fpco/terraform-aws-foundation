// Instance ID(s) of the server(s)
output "instance_ids" {
  value = ["${aws_instance.auto-recover.*.id}"]
}

// Private IP address(es) of the server(s)
output "private_ips" {
  value = ["${var.private_ips}"]
}
