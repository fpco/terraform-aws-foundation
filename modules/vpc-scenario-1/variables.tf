variable "name_prefix" {
  description = "Name to prefix various resources with"
  type        = string
}

variable "region" {
  description = "Region were VPC will be created"
  type        = string
}

variable "cidr" {
  description = "CIDR range of VPC. eg: 172.16.0.0/16"
  type        = string
}

variable "public_subnet_cidrs" {
  type        = list(string)
  description = "A list of public subnet CIDRs to deploy inside the VPC."
}

variable "azs" {
  type        = list(string)
  description = "A list of Availaiblity Zones in the region"
}

variable "extra_tags" {
  type        = map(string)
  default     = {}
  description = "Extra tags that will be added to VPC, DHCP Options, Internet Gateway, Subnets and Routing Table."
}

variable "enable_dns_hostnames" {
  default     = true
  description = "boolean, enable/disable VPC attribute, enable_dns_hostnames"
  type        = bool
}

variable "enable_dns_support" {
  default     = true
  description = "boolean, enable/disable VPC attribute, enable_dns_support"
  type        = bool
}

variable "dns_servers" {
  default     = ["AmazonProvidedDNS"]
  description = "list of DNS servers"
  type        = list(string)
}

variable "map_on_launch" {
  default     = true
  description = "Map public subnet addresses to instances."
  type = bool
}
