/**
 * ## VPC w/ DHCP Options
 *
 * This module creates the basic VPC and DHCP options resources.
 * Use this module in combination with the `subnet`, `nat-gateway`,
 * and related network modules.
 *
 */

resource "aws_vpc" "main" {
  cidr_block           = "${var.cidr}"
  enable_dns_hostnames = "${var.enable_dns_hostnames}"
  enable_dns_support   = "${var.enable_dns_support}"

  tags = "${merge(map("Name", "${var.name_prefix}"), "${var.extra_tags}")}"
}

// move these two into their own module? or combine with aws_vpc in a module?
resource "aws_vpc_dhcp_options" "main" {
  domain_name         = "${var.region}.compute.internal"
  domain_name_servers = "${var.dns_servers}"
  ntp_servers         = "${var.ntp_servers}"

  tags = "${merge(map("Name", "${var.name_prefix}"), "${var.extra_tags}")}"
}

resource "aws_vpc_dhcp_options_association" "main" {
  vpc_id          = "${aws_vpc.main.id}"
  dhcp_options_id = "${aws_vpc_dhcp_options.main.id}"
}
