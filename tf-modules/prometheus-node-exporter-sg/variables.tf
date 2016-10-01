variable "name" {
    description = "A short identifier to use in the name of the security group"
}
variable "access_key" {
    description = "AWS key id"
    default = ""
}
variable "secret_key" {
    description = "AWS key secret"
    default = ""
}
variable "region" {
    description = "AWS region to deploy to"
    default = ""
}
variable "vpc_id" {
    description = "The ID of the VPC to deploy to"
}
variable "cidr_blocks" {
    description = "The list of CIDR IP blocks allowed to access the node-exporter (as a string)"
}

