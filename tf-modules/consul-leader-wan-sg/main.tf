/**
 *## Consul Leaders WAN Security Group
 *
 *Boxed set of security group rules for use with a cluster of consul leaders.
 *These are the ingress rules for leaders connecting to other leaders across
 *the WAN.
 *
 *See `consul-agents-sg` for Agents, or `consul-leaders-sg` for the
 *intra-datacenter leader communication.
 *
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to access the consul ports"
  type        = "list"
}

variable "description" {
  description = "use this string to generate a description for the SG rules"
  default     = "Allow ingress, consuls WAN serf port 8302"
}


# TCP/UDP for serf WAN communication
resource "aws_security_group_rule" "serf_wan_tcp" {
  type              = "ingress"
  description       = "${var.description} (TCP)"
  from_port         = "8302"
  to_port           = "8302"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "serf_wan_udp" {
  type              = "ingress"
  description       = "${var.description} (UDP)"
  from_port         = "8302"
  to_port           = "8302"
  protocol          = "udp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
