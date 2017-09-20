/**
 *## Open Ingress Security Group
 *
 * Create a simple and reusable security group that opens all ports for
 * both TCP and UDP (unrestricted).
 *
 */

resource "aws_security_group" "main" {
  name   = "${var.name_prefix}-${var.name_suffix}"
  vpc_id = "${module.vpc.vpc_id}"

  tags {
    Name = "${var.name_prefix}-${var.name_suffix}"
  }

  ingress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["${var.cidr_blocks}"]
  }
}

//`id` exported from `aws_security_group`
output "id" {
  value = "${aws_security_group.main.id}"
}

//`name` exported from `aws_security_group`
output "name" {
  value = "${aws_security_group.main.name}"
}
