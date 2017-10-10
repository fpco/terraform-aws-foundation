output "dns" {
  value = "${aws_instance.ec2-nat.public_dns}"
}

output "route_table_id" {
  value = "${aws_route_table.ec2-nat.id}"
}
