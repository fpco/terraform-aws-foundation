output "id" {
    value = "${aws_nat_gateway.nat.id}"
}
output "allocation_id" {
    value = "${aws_nat_gateway.nat.allocation_id}"
}
output "subnet_id" {
    value = "${aws_nat_gateway.nat.subnet_id}"
}
output "network_interface_id" {
    value = "${aws_nat_gateway.nat.network_interface_id}"
}
output "private_ip" {
    value = "${aws_nat_gateway.nat.private_ip}"
}
output "public_ip" {
    value = "${aws_nat_gateway.nat.public_ip}"
}
