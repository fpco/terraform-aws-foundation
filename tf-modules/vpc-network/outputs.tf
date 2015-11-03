output "id" {
    value = "${aws_vpc.core.id}"
}
output "cidr_block" {
    value = "${aws_vpc.core.cidr_block}"
}
output "route_table_id" {
    value = "${aws_route_table.core.id}"
}
output "igw_id" {
    value = "${aws_internet_gateway.core.id}"
}
