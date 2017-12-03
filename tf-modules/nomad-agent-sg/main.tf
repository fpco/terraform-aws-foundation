/**
 * ## Nomad Agent Security Group Rule
 *
 * Attach a security group rule for the Nomad agents, allowing port 4646.
 *
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed access to the agent on port 4646"
  type        = "list"
}

variable "description" {
  description = "use this string to generate a description for the SG rules"
  default     = "Allow ingress, nomads HTTP port 4646"
}

# open port 4646 (nomad http) tcp
resource "aws_security_group_rule" "nomad_agent_tcp" {
  type              = "ingress"
  description       = "${var.description} (TCP)"
  from_port         = "4646"
  to_port           = "4646"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
