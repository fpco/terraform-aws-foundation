variable "name" {
  description = "The name of this network"
  type        = string
}

variable "vpc_cidr_prefix" {
  description = "The prefix to the VPC CIDR block, eg: 10.100"
  type        = string
}

variable "enable_dns_hostnames" {
  default     = true
  description = "Boolean to set the `enable_dns_hostnames` flag for the VPC"
  type        = string
}

