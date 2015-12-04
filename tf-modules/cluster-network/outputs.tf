output "az_a" {
    value = "${aws_subnet.a.availability_zone}"
}
output "id_a" {
    value = "${aws_subnet.a.id}"
}
output "cidr_a" {
    value = "${aws_subnet.a.cidr_block}"
}
#
output "az_c" {
    value = "${aws_subnet.c.availability_zone}"
}
output "id_c" {
    value = "${aws_subnet.c.id}"
}
output "cidr_c" {
    value = "${aws_subnet.c.cidr_block}"
}
