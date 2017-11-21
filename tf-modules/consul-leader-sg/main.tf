/**
 *## Ingress Rules for Consul Leaders
 *
 *Set of security group ingress rules for use with a cluster of consul leaders.
 *These are the ingress rules for leaders connecting to other leaders.
 *
 *See `consul-agents-sg` for Agents, or `consul-leaders-wan-sg` for the
 *cross-WAN leader communication.
 *
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to access the consul ports"
  type        = "list"
}

# Server RPC, used by servers to handle incoming requests from other agents.
resource "aws_security_group_rule" "server_rpc_tcp" {
  type              = "ingress"
  from_port         = "8300"
  to_port           = "8300"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "server_rpc_udp" {
  type              = "ingress"
  from_port         = "8300"
  to_port           = "8300"
  protocol          = "udp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

# Serf LAN, used to handle gossip in the LAN. TCP and UDP.
resource "aws_security_group_rule" "serf_lan_tcp" {
  type              = "ingress"
  from_port         = "8301"
  to_port           = "8301"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "serf_lan_udp" {
  type              = "ingress"
  from_port         = "8301"
  to_port           = "8301"
  protocol          = "udp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

# consul CLI RPC, used by all agents to handle RPC from the CLI. TCP only.
resource "aws_security_group_rule" "cli_rpc_tcp" {
  type              = "ingress"
  from_port         = "8400"
  to_port           = "8400"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

# HTTP API, used by clients to talk to the HTTP API. TCP only.
resource "aws_security_group_rule" "http_tcp" {
  type              = "ingress"
  from_port         = "8500"
  to_port           = "8500"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

# DNS Interface, used to resolve DNS queries. TCP and UDP.
resource "aws_security_group_rule" "dns_tcp" {
  type              = "ingress"
  from_port         = "8600"
  to_port           = "8600"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "dns_udp" {
  type              = "ingress"
  from_port         = "8600"
  to_port           = "8600"
  protocol          = "udp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
