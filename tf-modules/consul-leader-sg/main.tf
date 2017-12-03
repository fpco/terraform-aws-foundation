/**
 * ## Ingress Rules for Consul Leaders
 * 
 * Set of security group ingress rules for use with a cluster of consul leaders.
 * These are the ingress rules for leaders connecting to other leaders.
 * 
 * See `consul-agents-sg` for Agents, or `consul-leaders-wan-sg` for the
 * cross-WAN leader communication.
 *
  * ### Example
 * 
 * ```
 * # Security Group for nomad workers
 * module "leaders-sg" {
 *   source      = "../tf-modules/security-group-base"
 *   name        = "${var.name}-leaders"
 *   description = "security group for consul leader instances in the private subnet"
 *   vpc_id      = "${module.vpc.vpc_id}"
 * }
 *
 * module "leaders-consul-leader-rules" {
 *   source            = "../tf-modules/consul-leader-sg"
 *   cidr_blocks       = ["${var.vpc_cidr}"]
 *   security_group_id = "${module.leaders-sg.id}"
 * }
 *
 * ...
 * 
 * module "my-cluster" {
 *   source             = "../tf-modules/asg"
 *   ...
 *   security_group_ids = ["${module.leaders-sg.id}"]
 * }
 * ```
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
  default     = "Allow ingress, consul leaders"
}

# Server RPC, used by servers to handle incoming requests from other agents.
resource "aws_security_group_rule" "server_rpc_tcp" {
  type              = "ingress"
  description       = "${var.description} server rpc port 8300 (TCP)"
  from_port         = "8300"
  to_port           = "8300"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "server_rpc_udp" {
  type              = "ingress"
  description       = "${var.description} server rpc port 8300 (UDP)"
  from_port         = "8300"
  to_port           = "8300"
  protocol          = "udp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

# Serf LAN, used to handle gossip in the LAN. TCP and UDP.
resource "aws_security_group_rule" "serf_lan_tcp" {
  type              = "ingress"
  description       = "${var.description} serf LAN port 8301 (TCP)"
  from_port         = "8301"
  to_port           = "8301"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "serf_lan_udp" {
  type              = "ingress"
  description       = "${var.description} serf LAN port 8301 (UDP)"
  from_port         = "8301"
  to_port           = "8301"
  protocol          = "udp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

# HTTP API, used by clients to talk to the HTTP API. TCP only.
resource "aws_security_group_rule" "http_tcp" {
  type              = "ingress"
  description       = "${var.description} HTTP API port 8500 (TCP)"
  from_port         = "8500"
  to_port           = "8500"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

# DNS Interface, used to resolve DNS queries. TCP and UDP.
resource "aws_security_group_rule" "dns_tcp" {
  type              = "ingress"
  description       = "${var.description} DNS port 8600 (TCP)"
  from_port         = "8600"
  to_port           = "8600"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "dns_udp" {
  type              = "ingress"
  description       = "${var.description} DNS port 8600 (UDP)"
  from_port         = "8600"
  to_port           = "8600"
  protocol          = "udp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
