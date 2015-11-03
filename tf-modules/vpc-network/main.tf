# THE VPC with an internet gateway
resource "aws_vpc" "core" {
    cidr_block = "${var.vpc_cidr_prefix}.0.0/16"
    enable_dns_hostnames = "${var.enable_dns_hostnames}"
    tags {
        Name = "${var.name}"
    }
}
resource "aws_internet_gateway" "core" {
    vpc_id = "${aws_vpc.core.id}"
    tags {
        Name = "${var.name}"
        Region = "${var.region}"
    }
}
resource "aws_route_table" "core" {
    vpc_id = "${aws_vpc.core.id}"
    route {
        cidr_block = "0.0.0.0/0"
        gateway_id = "${aws_internet_gateway.core.id}"
    }
    tags {
        Name = "${var.name}"
        Region = "${var.region}"
    }
}
