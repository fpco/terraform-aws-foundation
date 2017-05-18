variable "name_prefix" {
  description = "Prefix to use when naming the SG"
}
variable "vpc_id" {
  description = "ID of VPC to associate SG with"
}
variable "cidr_blocks" {
  description = "List of CIDR block ranges that the SG allows ingress from"
  type        = "list"
}
variable "dns_port" {
  description = "Port where DNS is listening"
  default     = "53"
}
# Security group for DNS servers
resource "aws_security_group" "main" {
  name        = "${var.name_prefix}-dns-server"
  vpc_id      = "${var.vpc_id}"

  tags {
    Name = "${var.name_prefix}-dns-server"
  }

  ingress {
    from_port   = "${var.dns_port}"
    to_port     = "${var.dns_port}"
    protocol    = "tcp"
    cidr_blocks = ["${var.cidr_blocks}"]
  }

  ingress {
    from_port   = "${var.dns_port}"
    to_port     = "${var.dns_port}"
    protocol    = "udp"
    cidr_blocks = ["${var.cidr_blocks}"]
  }

}
// ID of the Security Group created
output "id" {
  value = "${aws_security_group.main.id}"
}
