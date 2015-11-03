output "id_a" {
    value = "${aws_subnet.a.id}"
}
output "cidr_a" {
    value = "${aws_subnet.a.cidr_block}"
}
output "id_c" {
    value = "${aws_subnet.c.id}"
}
output "cidr_c" {
    value = "${aws_subnet.c.cidr_block}"
}
