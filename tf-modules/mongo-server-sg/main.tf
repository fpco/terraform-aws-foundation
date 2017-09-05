/**
 * ## MongoDB Server Security Group
 *
 * Security group for use with MongoDB servers.
 */
variable "name" {
    description = "The name prefix for this SSH security group"
}
variable "name_suffix" {
    default = "mongo-server"
    description = "The suffix in forming the name for this security group"
}
variable "allowed_cidr_blocks" {
    description = "The CIDR block to allow access to"
}
variable "vpc_id" {
    description = "The ID of the VPC to deploy to"
}
resource "aws_security_group" "mongo" {
    name = "${var.name}-${var.name_suffix}"
    vpc_id = "${var.vpc_id}"
    tags {
        Name = "${var.name}-${var.name_suffix}"
        Description = "Allow access to mongodb servers in ${var.name}"
    }
    # mongodb
    ingress {
        from_port = 27017
        to_port = 27017
        protocol = "tcp"
        cidr_blocks = ["${split(",", replace(var.allowed_cidr_blocks, " ", ""))}"]
    }
    ingress {
        from_port = 28017
        to_port = 28017
        protocol = "tcp"
        cidr_blocks = ["${split(",", replace(var.allowed_cidr_blocks, " ", ""))}"]
    }
}
//`id` exported from `aws_security_group`
output "id" {
    value = "${aws_security_group.mongo.id}"
}
//`name` exported from `aws_security_group`
output "name" {
    value = "${aws_security_group.mongo.name}"
}
