/**
 *##Nomad Agent Task/Service/Worker Security Group
 *
 * Security group rules for the task/service ports for Nomad workers (agents).
 *
 * NOTE: need to make the agent port range (20000 to 60000) a variable. And enable
 * disable TCP or UDP separately with count.
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed access to the agent's worker ports"
  type        = "list"
}

# open worker port range, tcp and udp
resource "aws_security_group_rule" "nomad_worker_tcp" {
  type              = "ingress"
  from_port         = "20000"
  to_port           = "32000"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "nomad_worker_udp" {
  type              = "ingress"
  from_port         = "20000"
  to_port           = "32000"
  protocol          = "udp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
