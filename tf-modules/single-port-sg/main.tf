/**
 * ## Single Port Security Group Rule
 *
 * Create an `aws_security_group_rule` to allow ingress on some port.
 *
 * TODO: support both TCP and UDP, use count to enable/disable.
 *
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "List of CIDR block ranges that the SG allows ingress from"
  type        = "list"
}

variable "port" {
  description = "The port to open"
}

variable "protocol" {
  description = "The protocol, specify either `tcp` or `udp`"
  default     = "tcp"
}

variable "description" {
  description = "Use this string to add a description for the SG rule"
}


# add an ingress rule
resource "aws_security_group_rule" "ingress" {
  type              = "ingress"
  description       = "${var.description}"
  from_port         = "${var.port}"
  to_port           = "${var.port}"
  protocol          = "${var.protocol}"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

