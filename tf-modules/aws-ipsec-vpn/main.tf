/**
 * ## AWS IPSEC VPN
 *
 * This module packages the various resources needed to setup an IPSEC VPN
 * on AWS:
 *
 * * `aws_vpn_gateway`
 * * `aws_customer_gateway`
 * * `aws_vpn_connection`
 * * `aws_vpn_connection_route`
 *
 * See the [`vpc-scenario-4` module](https://github.com/fpco/fpco-terraform-aws/tree/master/tf-modules/vpc-scenario-4)
 * in this repo for an example that uses this module.
 *
 */

variable "name" {
  description = "Used to name the various VPN resources"
  type        = "string"
}

variable "vpc_id" {
  description = "ID of the VPC to associate the VPN with"
  type        = "string"
}

variable "remote_device_ip" {
  description = "The public IP address of the remote (client) device"
  type        = "string"
}

variable "static_routes" {
  description = "The list of CIDR blocks to create static routes for"
  type        = "list"
}

variable "extra_tags" {
  description = "Extra tags to append to various AWS resources"
  default     = {}
  type        = "map"
}

resource "aws_vpn_gateway" "main" {
  vpc_id = "${var.vpc_id}"
  tags   = "${merge(map("Name", "${var.name}"), "${var.extra_tags}")}"
}

resource "aws_customer_gateway" "main" {
  ip_address = "${var.remote_device_ip}"
  bgp_asn    = "65000" # required, but I don't think it's used with a static config
  type       = "ipsec.1"
  tags       = "${merge(map("Name", "${var.name}"), "${var.extra_tags}")}"
}

resource "aws_vpn_connection" "main" {
  vpn_gateway_id      = "${aws_vpn_gateway.main.id}"
  customer_gateway_id = "${aws_customer_gateway.main.id}"
  type                = "ipsec.1"
  static_routes_only  = true
  tags                = "${merge(map("Name", "${var.name}"), "${var.extra_tags}")}"
}

resource "aws_vpn_connection_route" "main" {
  count                  = "${length(var.static_routes)}"
  destination_cidr_block = "${element(var.static_routes, count.index)}"
  vpn_connection_id      = "${aws_vpn_connection.main.id}"
}
