//`id` exported from `aws_vpc`
output "id" {
  value = "${aws_vpc.core.id}"
}

//`cidr_block` exported from `aws_vpc`
output "cidr_block" {
  value = "${aws_vpc.core.cidr_block}"
}

//`id` exported from `aws_route_table`
output "route_table_id" {
  value = "${aws_route_table.core.id}"
}

//`id` exported from `aws_internet_gateway`
output "igw_id" {
  value = "${aws_internet_gateway.core.id}"
}
