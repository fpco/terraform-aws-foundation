/**
 * ## Security Group Base
 *
 * Create the `aws_security_group` that we will then go attach a bunch of
 * `aws_security_group_rule`s to.
 *
 * Note that, when setting `aws_security_group.name`, the AWS API will require
 * removing and recreating the resource if the `name` is changed.
 *
 */

variable "name" {
  description = "security group `name`"
  type        = string
}

variable "description" {
  description = "security group `description`"
  type        = string
}

variable "vpc_id" {
  description = "ID of VPC to associate SG with"
  type        = string
}

variable "extra_tags" {
  description = "map of name,value pairs to tag the security group (append to Name tag)"
  default     = {}
  type        = map(string)
}

resource "aws_security_group" "main" {
  name        = var.name
  description = var.description
  vpc_id      = var.vpc_id

  tags = merge(
    {
      "Name" = var.name
    },
    var.extra_tags,
  )
}

output "id" {
  value       = aws_security_group.main.id
  description = "ID of the Security Group created"
}

output "name" {
  value       = aws_security_group.main.name
  description = "Name of the Security Group created"
}

