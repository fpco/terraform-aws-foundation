variable "name" {
    default = "test"
    description = "The name of this deployment, should be unique"
}
variable "key_file" {
    default = "~/.ssh/consul-leader-asg"
    description = "The full path to the private key for use with SSH"
}
variable "key_name" {
    default = "consul-leader-asg"
    description = "The name of the (AWS) SSH key to associate with the instance"
}
variable "ami" {
    default = ""
    description = "The AMI to use when creating new instances"
}
variable "ssh_pubkey" {
    description = "The contents of the public key to be dropped into the AWS instances"
}

variable "access_key" {
    description = "AWS key id"
}
variable "secret_key" {
    description = "AWS key secret"
}
variable "token" {
    default = ""
    description = "MFA token for AWS provider"
}
variable "consul_secret_key" {
    description = "Secret key to secure cluster communication, generate this with `consul keygen`"
}

variable "consul_master_token" {
    description = "Master token for the ACL system in Consul"
}

variable "cidr_prefix_leader_a" {
    default = "10.100.3"
    description = "The prefix to use for the CIDR block and IP details in the formula, subnet a"
}
variable "cidr_prefix_leader_c" {
    default = "10.100.4"
    description = "The prefix to use for the CIDR block and IP details in the formula, subnet c"
}
variable "cidr_minions_a" {
    default = "10.100.5.0/24"
    description = "The CIDR block for minion subnet a"
}
variable "cidr_minions_c" {
    default = "10.100.6.0/24"
    description = "The CIDR block for minion subnet c"
}
variable "cidr_redis_a" {
    default = "10.100.7.0/24"
    description = "The CIDR block for redis elasticache subnet a"
}
variable "cidr_redis_c" {
    default = "10.100.8.0/24"
    description = "The CIDR block for redis elasticache subnet c"
}
variable "cidr_manage_a" {
    default = "10.100.9.0/28"
    description = "The CIDR block for management cluster subnet a"
}
variable "cidr_manage_c" {
    default = "10.100.10.0/28"
    description = "The CIDR block for management cluster subnet c"
}
variable "region" {
    description = "AWS region to deploy to"
}
variable "vpc_cidr_prefix" {
    default = "10.100"
    description = "The prefix to the CIDR block for the VPC"
}
variable "public_ip" {
    default = "true"
    description = "Boolean flag to enable/disable `map_public_ip_on_launch` in each `aws_subnet`"
}
