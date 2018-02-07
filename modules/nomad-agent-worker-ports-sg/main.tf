/**
 * ## Nomad Agent Task/Service/Worker Security Group Rules
 *
 * Attach security group rules to open the task/service ports on the Nomad workers
 * (agents).
 *
 * NOTE: Also consider enabling/disabling TCP or UDP separately with count.
 *
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed access to the agent's worker ports"
  type        = "list"
}

variable "description" {
  description = "use this string to generate a description for the SG rules"
  default     = "Allow ingress, to ports 20000 to 32000, for nomad dynamic assignment"
}

# open worker port range, tcp and udp
resource "aws_security_group_rule" "nomad_worker_tcp" {
  type              = "ingress"
  description       = "${var.description} (TCP)"
  from_port         = "20000"
  to_port           = "32000"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "nomad_worker_udp" {
  type              = "ingress"
  description       = "${var.description} (UDP)"
  from_port         = "20000"
  to_port           = "32000"
  protocol          = "udp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
