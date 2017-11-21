variable "security_group_id" {
  description = "security group to attach the ingress rules to"
}

variable "cidr_blocks" {
  description = "List of CIDR block ranges that the SG allows ingress from"
  type        = "list"
}

variable "port" {
  description = "TCP port to open"
}

variable "protocol" {
  description = "tcp/udp"
  default     = "tcp"
}


# add an ingress rule 
resource "aws_security_group_rule" "ingress" {
  type              = "ingress"
  from_port         = "${var.port}"
  to_port           = "${var.port}"
  protocol          = "${var.protocol}"
  cidr_blocks       = ["${var.cidr_blocks}"]
  security_group_id = "${var.security_group_id}"
}

