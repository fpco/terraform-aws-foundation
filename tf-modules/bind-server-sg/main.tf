/*
 * ## BIND (DNS) Server Security Group Rule
 *
 * Defines `aws_security_group_rule`s for DNS servers
 *
 */

variable "cidr_blocks" {
  description = "List of CIDR block ranges that the SG allows ingress from"
  type        = "list"
}

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "dns_port" {
  description = "Port where DNS is listening"
  default     = "53"
}

variable "description" {
  description = "use this string to generate a description for the SG rules"
  default     = "Allow ingress, DNS port"
}

# ingress rules for DNS servers
resource "aws_security_group_rule" "dns_tcp" {
  type              = "ingress"
  description       = "${var.description} ${var.dns_port} (TCP)"
  from_port         = "${var.dns_port}"
  to_port           = "${var.dns_port}"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "dns_udp" {
  type              = "ingress"
  description       = "${var.description} ${var.dns_port} (UDP)"
  from_port         = "${var.dns_port}"
  to_port           = "${var.dns_port}"
  protocol          = "udp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
