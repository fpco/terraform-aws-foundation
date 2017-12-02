// List of instance ID(s) of the server(s)
output "instance_ids" {
  value = ["${aws_instance.auto-recover.*.id}"]
}

// List of private IP address(es) of the server(s)
output "private_ips" {
  value = ["${var.private_ips}"]
}

// List of public IP address(es) for the server(s)
output "public_ips" {
  value = ["${aws_instance.auto-recover.*.public_ip}"]
}
