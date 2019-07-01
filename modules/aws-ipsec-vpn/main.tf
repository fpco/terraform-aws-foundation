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
 * See the [`vpc-scenario-4` module](https://github.com/fpco/terraform-aws-foundation/tree/master/modules/vpc-scenario-4)
 * in this repo for an example that uses this module.
 *
 */

variable "name" {
  description = "Used to name the various VPN resources"
  type        = string
}

variable "vpc_id" {
  description = "ID of the VPC to associate the VPN with"
  type        = string
}

variable "remote_device_ip" {
  description = "The public IP address of the remote (client) device"
  type        = string
}

variable "bgp_asn" {
  description = "The gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN)"
  default     = "65000"
  type        = string
}

variable "connection_type" {
  description = "The type of VPN connection. The only type AWS supports at this time is `ipsec.1`"
  default     = "ipsec.1"
  type        = string
}

variable "static_routes_only" {
  description = "Whether the VPN connection uses static routes exclusively. Static routes must be used for devices that don't support BGP"
  default     = true
  type        = string
}

variable "static_routes" {
  description = "The list of CIDR blocks to create static routes for"
  type        = list(string)
  default     = []
}

variable "extra_tags" {
  description = "Extra tags to append to various AWS resources"
  default     = {}
  type        = map(string)
}

resource "aws_vpn_gateway" "main" {
  vpc_id = var.vpc_id
  tags = merge(
    {
      "Name" = var.name
    },
    var.extra_tags,
  )
}

resource "aws_customer_gateway" "main" {
  ip_address = var.remote_device_ip
  bgp_asn    = var.bgp_asn
  type       = "ipsec.1"
  tags = merge(
    {
      "Name" = var.name
    },
    var.extra_tags,
  )
}

resource "aws_vpn_connection" "main" {
  vpn_gateway_id      = aws_vpn_gateway.main.id
  customer_gateway_id = aws_customer_gateway.main.id
  type                = var.connection_type
  static_routes_only  = var.static_routes_only
  tags = merge(
    {
      "Name" = var.name
    },
    var.extra_tags,
  )

  /**
  * This is needed for FIPS VPN enpoints, as an un-documented `type` must be
  * provided, but AWS does not report the type back correctly. This causes an
  * erroneous change to be detected in TF.
  */
  lifecycle {
    ignore_changes = [type]
  }
}

resource "aws_vpn_connection_route" "main" {
  count                  = length(var.static_routes)
  destination_cidr_block = element(var.static_routes, count.index)
  vpn_connection_id      = aws_vpn_connection.main.id
}

