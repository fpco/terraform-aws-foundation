/**
 * ## Open Ingress Security Group Rule
 *
 * Create a simple and reusable security group rule that opens all ports for
 * both TCP and UDP traffic (unrestricted).
 *
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to access the etcd ports"
  type        = "list"
}

variable "description" {
  description = "use this string to generate a description for the SG rules"
  default     = "OPEN ingress, all ports, all protocols"
}

# open ingress!
resource "aws_security_group_rule" "open_ingress" {
  type              = "ingress"
  description       = "${var.description}"
  from_port         = "0"
  to_port           = "0"
  protocol          = "-1"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
