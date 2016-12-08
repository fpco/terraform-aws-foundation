variable "name" {
    default = ""
    description = "The name prefix for this SSH security group"
}
variable "allowed_cidr_blocks" {
    default = "0.0.0.0/0"
    description = "The CIDR block to allow access to"
}
variable "vpc_id" {
    description = "The ID of the VPC to deploy to"
}
