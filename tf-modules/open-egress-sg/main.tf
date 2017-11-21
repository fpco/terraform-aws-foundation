/**
 *## Open Egress Security Group Rule
 *
 * Create a simple and reusable security group rule for "open egress".
 */

variable "security_group_id" {
  description = "security group to attach the egress rule to"
}

variable "cidr_blocks" {
  description = "Allow egress to these CIDR blocks"
  default     = ["0.0.0.0/0"]
}

# unrestricted outbound (egress)
resource "aws_security_group_rule" "open_ingress" {
  type              = "egress"
  from_port         = "0"
  to_port           = "0"
  protocol          = "-1"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
