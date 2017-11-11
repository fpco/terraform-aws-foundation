variable "name" {
  default     = ""
  description = "The name of this network"
}

variable "description" {
  default     = ""
  description = "The description to give the subnets created for this network"
}

variable "cidr_a" {
  default     = ""
  description = "The CIDR block for subnet a, eg: 10.100.7.0/24"
}

variable "cidr_c" {
  default     = ""
  description = "The CIDR block for subnet c, eg: 10.100.8.0/24"
}

variable "region" {
  description = "AWS region to deploy to"
  default     = ""
}

variable "vpc_id" {
  description = "The ID of the VPC to deploy to"
}

variable "route_table_id" {
  description = "The ID of the routing table to use"
}

variable "public_ip" {
  default     = "true"
  description = "Boolean flag to enable/disable `map_public_ip_on_launch` in each `aws_subnet`"
}
