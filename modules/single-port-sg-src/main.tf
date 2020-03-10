/**
 * ## Single Port Security Group Rule
 *
 * Create an `aws_security_group_rule` to allow ingress on some port.
 *
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
  type        = string
}

variable "source_security_group_id" {
  description = "The SG that this SG allows ingress from"
  type        = string
}

variable "description" {
  description = "Use this string to add a description for the SG rule"
  type        = string
}

variable "port" {
  description = "The port to open"
  type        = string
}

variable "tcp" {
  description = "true/false to enables the tcp ingress"
  default     = "true"
  type        = string
}

variable "udp" {
  description = "true/false to enables the udp ingress"
  default     = "false"
  type        = string
}

locals {
  tcp = "${var.tcp ? 1 : 0}"
  udp = "${var.udp ? 1 : 0}"
}

# ingress rule for tcp, if enabled
resource "aws_security_group_rule" "tcp_ingress" {
  count                    = local.tcp
  type                     = "ingress"
  description              = "${var.description} (tcp)"
  from_port                = var.port
  to_port                  = var.port
  protocol                 = "tcp"
  security_group_id        = var.security_group_id
  source_security_group_id = var.source_security_group_id
}

# ingress rule for udp, if enabled
resource "aws_security_group_rule" "udp_ingress" {
  count                    = local.udp
  type                     = "ingress"
  description              = "${var.description} (udp)"
  from_port                = var.port
  to_port                  = var.port
  protocol                 = "udp"
  security_group_id        = var.security_group_id
  source_security_group_id = var.source_security_group_id
}
