variable "name" {
  default     = ""
  description = "The name of this network"
  type = string
}

variable "description" {
  default     = ""
  description = "The description to give the subnets created for this network"
  type = string
}

variable "cidr_a" {
  default     = ""
  description = "The CIDR block for subnet a, eg: 10.100.7.0/24"
  type = string
}

variable "cidr_c" {
  default     = ""
  description = "The CIDR block for subnet c, eg: 10.100.8.0/24"
  type = string
}

variable "region" {
  description = "AWS region to deploy to"
  default     = ""
  type = string
}

variable "vpc_id" {
  description = "The ID of the VPC to deploy to"
  type = string
}

variable "route_table_id" {
  description = "The ID of the routing table to use"
  type = string
}

variable "public_ip" {
  default     = true
  description = "Boolean flag to enable/disable `map_public_ip_on_launch` in each `aws_subnet`"
  type = bool
}

