## AWS IPSEC VPN

This module packages the various resources needed to setup an IPSEC VPN
on AWS:

* `aws_vpn_gateway`
* `aws_customer_gateway`
* `aws_vpn_connection`
* `aws_vpn_connection_route`

See the [`vpc-scenario-4` module](https://github.com/fpco/terraform-aws-foundation/tree/master/modules/vpc-scenario-4)
in this repo for an example that uses this module.
