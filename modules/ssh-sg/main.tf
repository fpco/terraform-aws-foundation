/**
 * ## SSH Security Group Rule
 *
 * Attach a security group rule to allow inbound SSH for administration.
 *
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to access the SSH port"
  default     = ["0.0.0.0/0"]
}

variable "ssh_port" {
  description = "The port to use"
  default     = "22"
}

variable "description" {
  description = "use this string to generate a description for the SG rule"
  default     = "Allow ingress, SSH server port "
}

# Security group for etcd servers
resource "aws_security_group_rule" "ssh_tcp" {
  type              = "ingress"
  description       = "${var.description} ${var.ssh_port} (TCP)"
  from_port         = "${var.ssh_port}"
  to_port           = "${var.ssh_port}"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
