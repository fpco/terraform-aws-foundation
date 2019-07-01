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

variable "private_subnet_cidrs" {
  description = "A list of private subnet CIDRs to deploy inside the VPC. Should not be higher than public subnets count"
  default     = []
  type        = list(string)
}

variable "azs" {
  type        = list(string)
  description = "A list of Availaiblity Zones in the region"
}

# if the description here is confusing, see the semantics that `merge()` uses:
# https://www.terraform.io/docs/configuration/interpolation.html#merge-map1-map2-
variable "extra_tags" {
  description = "Extra tags that will be added to ALL RESOURCES, use the resource-specific variables if you need that level of control. Note that keys may be overwritten in cases where there are duplicates."
  default     = {}
  type        = map(string)
}

variable "vpc_extra_tags" {
  description = "Extra tags that will be added to VPC and DHCP Options. Note that duplicate keys will overwrite those from the extra_tags variable."
  default     = {}
  type        = map(string)
}

variable "public_gateway_extra_tags" {
  description = "Extra tags that will be added to Internet Gateway and public Routing Tables."
  default     = {}
  type        = map(string)
}

variable "public_subnet_extra_tags" {
  description = "Extra tags that will be added to public subnets."
  default     = {}
  type        = map(string)
}

variable "private_subnet_extra_tags" {
  description = "Extra tags that will be added to private subnets."
  default     = {}
  type        = map(string)
}

variable "nat_gateway_extra_tags" {
  description = "Extra tags that will be added to NAT gateway and routing tables for the private subnets"
  default     = {}
  type        = map(string)
}

# DNS
variable "enable_dns_hostnames" {
  default     = true
  description = "boolean, enable/disable VPC attribute, enable_dns_hostnames"
  type        = string
}

variable "enable_dns_support" {
  default     = true
  description = "boolean, enable/disable VPC attribute, enable_dns_support"
  type        = string
}

variable "dns_servers" {
  default     = ["AmazonProvidedDNS"]
  description = "list of DNS servers"
  type        = list(string)
}

