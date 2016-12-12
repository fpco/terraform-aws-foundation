//ID from `aws_vpc`
output "vpc_id" {
  value = "${aws_vpc.main.id}"
}
//List of IDs of public subnets
output "public_subnets" {
  value = ["${aws_subnet.public.*.id}"]
}
//List of IDs of routing tables
output "public_route_table_id" {
  value = "${aws_route_table.public.id}"
}
//ID of internet gateway
output "igw_id" {
  value = "${aws_internet_gateway.main.id}"
}
