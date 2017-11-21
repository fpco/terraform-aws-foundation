/*
 * ADD DOCS
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to access the etcd ports"
  type        = "list"
}

# Security group for etcd servers
resource "aws_security_group_rule" "etcd_server_tcp" {
  type              = "ingress"
  from_port         = "2379"
  to_port           = "2380"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
