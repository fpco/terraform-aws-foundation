/**
 * ## etcd Server Security Group Rules
 *
 * Attach an `aws_security_group_rule` to the specified security group. Allow
 * TCP on ports `2379` and `2380` for `etcd`.
 *
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to access the etcd ports"
  type        = "list"
}

variable "description" {
  description = "use this string to generate a description for the SG rules"
  default     = "Allow ingress, etcd server ports 2379 and 2380 (TCP)"
}

# Security group for etcd servers
resource "aws_security_group_rule" "etcd_server_tcp" {
  type              = "ingress"
  description       = "${var.description} (TCP)"
  from_port         = "2379"
  to_port           = "2380"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
