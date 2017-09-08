variable "name_prefix" {
  description = "Prefix to use when naming the SG"
}
variable "name_suffix" {
  description = "Suffix to use when naming the SG"
  default     = "etcd-server"
}
variable "vpc_id" {
  description = "ID of VPC to associate SG with"
}
variable "cidr_blocks" {
  description = "List of CIDR block ranges that the SG allows ingress from"
  type        = "list"
}
# Security group for DNS servers
resource "aws_security_group" "main" {
  name        = "${var.name_prefix}-${var.name_suffix}"
  vpc_id      = "${var.vpc_id}"

  tags {
    Name = "${var.name_prefix}-${var.name_suffix}"
  }

  ingress {
    from_port   = "2379"
    to_port     = "2380"
    protocol    = "tcp"
    cidr_blocks = ["${var.cidr_blocks}"]
  }
}
// ID of the Security Group created
output "id" {
  value = "${aws_security_group.main.id}"
}

