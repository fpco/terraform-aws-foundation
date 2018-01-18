/**
 * ## Ping Respond Security Group Rule
 *
 * Create a simple and reusable security group rule that allows
 * hosts to accept requests to and respond ping queries
 *
 * CIDR blocks provided are both hosts that can query this SG
 * AND the only hosts that can receive the responses
 */

variable "security_group_id" {
  description = "security group to attach rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to ping hosts with this security group"
  type        = "list"
}

resource "aws_security_group_rule" "ping_requests" {
  type              = "ingress"
  from_port         = "8"
  to_port           = "8"
  protocol          = "icmp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "ping_response" {
  type              = "egress"
  from_port         = "0"
  to_port           = "0"
  protocol          = "icmp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
