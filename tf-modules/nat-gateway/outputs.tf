//`id` exported from the `aws_nat_gateway`
output "id" {
    value = "${aws_nat_gateway.nat.id}"
}
//`allocation_id` exported from the `aws_nat_gateway`
output "allocation_id" {
    value = "${aws_nat_gateway.nat.allocation_id}"
}
//`subnet_id` exported from the `aws_nat_gateway`
output "subnet_id" {
    value = "${aws_nat_gateway.nat.subnet_id}"
}
//`network_interface_id` exported from the `aws_nat_gateway`
output "network_interface_id" {
    value = "${aws_nat_gateway.nat.network_interface_id}"
}
//`private_ip` exported from the `aws_nat_gateway`
output "private_ip" {
    value = "${aws_nat_gateway.nat.private_ip}"
}
//`public_ip` exported from the `aws_nat_gateway`
output "public_ip" {
    value = "${aws_nat_gateway.nat.public_ip}"
}
