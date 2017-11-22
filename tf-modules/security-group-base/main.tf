/*
 * ## Security Group Base
 *
 * Add DOCS
 *
 */

variable "name" {
  description = "security group `name`"
  type        = "string"
}

variable "description" {
  description = "security group `description`"
  type        = "string"
}

variable "vpc_id" {
  description = "ID of VPC to associate SG with"
  type        = "string"
}

variable "extra_tags" {
  description = "map of name,value pairs to tag the security group (append to Name tag)"
  type        = "map"
}

resource "aws_security_group" "main" {
  name        = "${var.name}"
  description = "${var.description}"
  vpc_id      = "${var.vpc_id}"

  tags = "${merge(map("Name", "${var.name}"), "${var.extra_tags}")}"
}

// ID of the Security Group created
output "id" {
  value = "${aws_security_group.main.id}"
}

// Name of the Security Group created
output "name" {
  value = "${aws_security_group.main.name}"
}
