output "ids" {
  value = ["${aws_nat_gateway.nat.*.id}"]
}

output "allocation_ids" {
  value = ["${aws_nat_gateway.nat.*.allocation_id}"]
}

output "subnet_ids" {
  value = ["${aws_nat_gateway.nat.*.subnet_id}"]
}

output "network_interface_ids" {
  value = ["${aws_nat_gateway.nat.*.network_interface_id}"]
}

output "private_ips" {
  value = ["${aws_nat_gateway.nat.*.private_ip}"]
}

output "public_ips" {
  value = ["${aws_nat_gateway.nat.*.public_ip}"]
}
