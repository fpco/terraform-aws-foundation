#
variable "name" {
    default = "packer"
    description = "The name of the environment to deploy to (beta/prod/etc)"
}
#
variable "region" {
    default = "us-west-1"
    description = "The AWS region to deploy to"
}
variable "az" {
    default = "a"
    description = "The AWS Availability Zone (AZ) to create a subnet in"
}
variable "vpc_cidr_prefix" {
    default = "10.72"
    description = "The IP prefix to the CIDR block assigned to the VPC"
}

#
variable "key_name" {
    default = "packer"
    description = "The AWS SSH pub key to use in granting access to the instance"
}
#
variable "key_file" {
    default = "./packer"
    description = "The path to the SSH private key to use connecting to the instance"
}
#
variable "ssh_pubkey" {
    default = ""
    description = "The contents of the PUBLIC KEY, to upload to AWS"
}
#
variable "ssh_user" {
    default = "ubuntu"
    description = "The SSH user to connect to the instance with"
}
#
variable "access_key" {
    default = ""
}
variable "secret_key" {
    default = ""
}
variable "token" {
    default = ""
}
