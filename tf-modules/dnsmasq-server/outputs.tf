// Instance ID(s) of the DNS server(s)
output "instance_ids" {
  value = ["${aws_instance.dnsmasq.*.id}"]
}

// Private IP address(es) of the DNS server(s)
output "private_ips" {
  value = ["${var.private_ips}"]
}
