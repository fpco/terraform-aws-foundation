/**
 * ## Ingress Rules for Consul Agents
 * 
 * This module creates `aws_security_group_rule` resources, defining an ingress
 * rule to allow port `8301`, for TCP and UDP each. Use with a security
 * group on any nodes you wish to use the consul agent on.
 * 
 * 
 * ### Example
 * 
 * ```
 * # Security Group for nomad workers
 * module "workers-sg" {
 *   source      = "../tf-modules/security-group-base"
 *   name        = "${var.name}-workers"
 *   description = "security group for worker instances in the private subnet"
 *   vpc_id      = "${module.vpc.vpc_id}"
 * }
 *
 * module "workers-consul-agent-rule" {
 *   source            = "../tf-modules/consul-agent-sg"
 *   cidr_blocks       = ["${var.vpc_cidr}"]
 *   security_group_id = "${module.workers-sg.id}"
 * }
 *
 * ...
 * 
 * module "my-cluster" {
 *   source             = "../tf-modules/asg"
 *   ...
 *   security_group_ids = ["${module.workers-sg.id}"]
 * }
 * ```
 *
 */

# ingress rules for consul agents. Required by all agents.

variable "security_group_id" {
  description = "ID of the security group to attach the ingress rules to"
  type        = "string"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to access the consul ports"
  type        = "list"
}

variable "description" {
  description = "use this string to generate a description for the SG rules"
  default     = "Allow ingress, consul LAN serf port 8301"
}

# Serf LAN, used to handle gossip in the LAN. TCP and UDP.
resource "aws_security_group_rule" "serf_lan_tcp" {
  type              = "ingress"
  description       = "${var.description} (TCP)"
  from_port         = "8301"
  to_port           = "8301"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "serf_lan_udp" {
  type              = "ingress"
  description       = "${var.description} (UDP)"
  from_port         = "8301"
  to_port           = "8301"
  protocol          = "udp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
