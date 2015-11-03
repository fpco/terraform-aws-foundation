variable "name" {
    default = ""
    description = "The name of this network"
}
variable "access_key" {
    description = "AWS key id"
}
variable "secret_key" {
    description = "AWS key secret"
}
variable "region" {
    description = "AWS region to deploy to"
}
variable "vpc_cidr_prefix" {
    default = ""
    description = "The prefix to the VPC CIDR block, eg: 10.100"
}
variable "enable_dns_hostnames" {
    default = "true"
    description = "Boolean to set the `enable_dns_hostnames` flag for the VPC"
}
