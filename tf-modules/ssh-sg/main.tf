/**
 *##SSH Security Group
 *
 *Security group to allow inbound SSH for administration
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to access the SSH port"
  type        = "list"
}

variable "ssh_port" {
  description = "The port to use"
  default     = "22"
}

# Security group for etcd servers
resource "aws_security_group_rule" "ssh_tcp" {
  type              = "ingress"
  from_port         = "${var.ssh_port}"
  to_port           = "${var.ssh_port}"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
