output "id" {
    value = "${aws_nat_gateway.gw.id}"
}
output "allocation_id" {
    value = "${aws_nat_gateway.gw.allocation_id}"
}
output "subnet_id" {
    value = "${aws_nat_gateway.gw.subnet_id}"
}
output "network_interface_id" {
    value = "${aws_nat_gateway.gw.network_interface_id}"
}
output "private_ip" {
    value = "${aws_nat_gateway.gw.private_ip}"
}
output "public_ip" {
    value = "${aws_nat_gateway.gw.public_ip}"
}
