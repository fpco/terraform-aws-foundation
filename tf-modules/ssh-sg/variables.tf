variable "name" {
    default = ""
    description = "The name prefix for this SSH security group"
}
variable "access_key" {
    description = "AWS key id"
    default = ""
}
variable "secret_key" {
    description = "AWS key secret"
    default = ""
}
variable "allowed_cidr_blocks" {
    default = "0.0.0.0/0"
    description = "The CIDR block to allow access to"
}
variable "region" {
    description = "AWS region to deploy to"
    default = ""
}
variable "vpc_id" {
    description = "The ID of the VPC to deploy to"
}
