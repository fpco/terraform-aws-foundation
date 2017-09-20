/**
 *## Open Egress Security Group
 *
 * Create a simple and reusable security group for "open egress".
 */
resource "aws_security_group" "main" {
  name   = "${var.name}-open-egress"
  vpc_id = "${var.vpc_id}"

  tags {
    Name        = "${var.name}-open-egress"
    Description = "Allow open egress for ${var.name}"
  }

  # unrestricted outbound
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
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
