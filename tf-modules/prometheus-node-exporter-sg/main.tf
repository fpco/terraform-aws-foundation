/**
 * ## Prometheus Node Exporter Security Group Rule
 *
 * Security group rule for the Prometheus node exporter.
 *
 */
variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to access the node exporter port"
  type        = "list"
}

# Security group ingress rule for the node exporter
resource "aws_security_group_rule" "node_exporter_tcp" {
  type              = "ingress"
  from_port         = "9100"
  to_port           = "9100"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
