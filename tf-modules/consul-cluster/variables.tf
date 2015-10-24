variable "name" {
    default = "test"
    description = "The name of this auto-scaling cluster, this should be unique"
}
variable "key_name" {
    default = "consul-asg"
    description = "The name of the (AWS) SSH key to associate with the instance"
}

variable "ami" {
    description = "The base AMI for each AWS instance created"
}

variable "iam_profile" {
    default = ""
    description = "The IAM profile to associate with AWS instances in the ASG"
}

variable "instance_type" {
    default = "t2.micro"
    description = "The type of AWS instance (size)"
}

variable "user_data" {
    default = ""
    description = "The user_data string to pass to cloud-init"
}

variable "inbound_security_group" {
    default = ""
    description = "ID of a security group, whitelist of client CIDR to consul minion, consul ports only"
}

variable "service_security_group" {
    default = ""
    description = "ID of a security group, this is the place to allow ports for your service"
}

variable "ssh_cidr_block" {
    default = "0.0.0.0/0"
    description = "The CIDR IP block to allow SSH connections from"
}

variable "max_nodes" {
    default = 9
    description = "The maximum number of nodes in each group"
}

variable "min_nodes" {
    default = 3
    description = "The minimum number of nodes in each group"
}

variable "desired_capacity" {
    default = 7
    description = "The desired number of nodes in each group"
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
variable "cidr_minions_a" {
    default = "10.100.7.0/24"
    description = "The CIDR block for subnet a"
}
variable "cidr_minions_c" {
    default = "10.100.8.0/24"
    description = "The CIDR block for subnet c"
}
variable "region" {
    description = "AWS region to deploy to"
}
variable "vpc_id" {
    description = "The ID of the experimental VPC to deploy to, depends on the AWS region"
}
variable "route_table_id" {
    description = "The ID of the experimental routing table to use, depends on the AWS region"
}
variable "public_ip" {
    default = "true"
    description = "Boolean flag to enable/disable `map_public_ip_on_launch` in each `aws_subnet`"
}
