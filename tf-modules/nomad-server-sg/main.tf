/**
 * ## Nomad Server Security Group
 *
 * Security group for the Nomad Servers/Leaders.
 *
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "server_cidr_blocks" {
  description = "The list of CIDR IP blocks where nomad servers run"
  type        = "list"
}

variable "worker_cidr_blocks" {
  description = "The list of CIDR IP blocks where nomad workers run"
  type        = "list"
}

variable "description" {
  description = "use this string to generate a description for the SG rules"
  default     = "Allow ingress, nomad server"
}

# nomad ports 4646 (http)/4647 (rpc), open to both workers and servers/leaders
# including 4646 here might lead it issues with it also being in the agent module
resource "aws_security_group_rule" "nomad_http_rpc_tcp" {
  type              = "ingress"
  description       = "${var.description} RPC and HTTP ports (TCP)"
  from_port         = "4646"
  to_port           = "4647"
  protocol          = "tcp"
  cidr_blocks       = ["${distinct(concat(var.server_cidr_blocks,
                                          var.worker_cidr_blocks))}"]
  security_group_id = "${var.security_group_id}"
}

# open port 4648 (nomad) tcp/udp for the leaders
resource "aws_security_group_rule" "nomad_serf_tcp" {
  type              = "ingress"
  description       = "${var.description} serf port (TCP)"
  from_port         = "4648"
  to_port           = "4648"
  protocol          = "tcp"
  cidr_blocks       = ["${var.server_cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "nomad_serf_udp" {
  type              = "ingress"
  description       = "${var.description} serf port (UDP)"
  from_port         = "4648"
  to_port           = "4648"
  protocol          = "udp"
  cidr_blocks       = ["${var.server_cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
