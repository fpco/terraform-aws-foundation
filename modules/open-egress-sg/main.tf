/**
 * ## Open Egress Security Group Rule
 *
 * Create a simple and reusable security group rule for "open egress", where
 * "open egress" means all ports, all protocols are allowed to the list of
 * CIDR blocks provided in.
 *
 */

variable "security_group_id" {
  description = "security group to attach the egress rule to"
}

variable "cidr_blocks" {
  description = "Allow egress to these CIDR blocks"
  default     = ["0.0.0.0/0"]
}

variable "description" {
  description = "use this string to generate a description for the SG rules"
  default     = "OPEN egress, all ports, all protocols"
}

# unrestricted outbound (egress)
resource "aws_security_group_rule" "open_egress" {
  type              = "egress"
  description       = "${var.description}"
  from_port         = "0"
  to_port           = "0"
  protocol          = "-1"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
