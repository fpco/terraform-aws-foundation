variable "is_govcloud" {
  description = "If this is running on GovCloud or not"
  default     = false
}

variable "vpc_id" {
  description = "VPC where this NAT instance should be created in"
}

variable "availability_zone" {
  description = "Availability zone for the NAT instance"
}

variable "name_prefix" {
  description = "Prefix for resources, usually project-related"
}

variable "extra_tags" {
  type = "map"
}

variable "public_subnet_ids" {
  type = "list"
}

variable "private_subnet_ids" {
  description = "Subnets that should have their traffic provided by the NAT instance"
  type        = "list"
}

variable "private_subnet_cidrs" {
  description = "CIDRs of private subnets for iptables configuration"
  type        = "list"
}

variable "aws_key_pair" {
  description = "Key to access NAT instance through SSH"
}
