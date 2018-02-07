/**
 * ## Ping Request Security Group Rule
 *
 * Create a simple and reusable security group rule that allows
 * hosts to ping hosts and receive responses from them
 *
 * CIDR blocks provided are both hosts that can be queried by this SG
 * AND the only hosts that can send responses
 *
 * It is obvious, but it should be noted that if you already have
 * open egress, you do not need this module. You are probably looking for
 * `ping-respond-sg` for hosts that can _receive_ ping requests and respond
 * instead.
 */

variable "security_group_id" {
  description = "security group to attach rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to be pinged by hosts with this security group"
  type        = "list"
}

resource "aws_security_group_rule" "ping_requests" {
  type              = "egress"
  from_port         = "8"
  to_port           = "8"
  protocol          = "icmp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "ping_response" {
  type              = "ingress"
  from_port         = "0"
  to_port           = "0"
  protocol          = "icmp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
