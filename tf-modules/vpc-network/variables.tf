variable "name" {
    default = ""
    description = "The name of this network"
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
variable "vpc_cidr_prefix" {
    default = ""
    description = "The prefix to the VPC CIDR block, eg: 10.100"
}
variable "enable_dns_hostnames" {
    default = "true"
    description = "Boolean to set the `enable_dns_hostnames` flag for the VPC"
}
