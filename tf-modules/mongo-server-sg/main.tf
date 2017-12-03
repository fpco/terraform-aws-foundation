/**
 * ## MongoDB Server Security Group
 *
 * Attach an `aws_security_group_rule` for each of MongoDB's two ports `27017`
 * and `28017`, to the security group specified.
 *
 */

variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to access mongod"
  type        = "list"
}

variable "description" {
  description = "use this string to generate a description for the SG rules"
  default     = "Allow ingress, mongod"
}
# Security group for mongod servers
resource "aws_security_group_rule" "mongo_tcp_1" {
  type              = "ingress"
  description       = "${var.description} client port 27017 (TCP)"
  from_port         = "27017"
  to_port           = "27017"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

resource "aws_security_group_rule" "mongo_tcp_2" {
  type              = "ingress"
  description       = "${var.description} admin port 28017 (TCP)"
  from_port         = "28017"
  to_port           = "28017"
  protocol          = "tcp"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}
