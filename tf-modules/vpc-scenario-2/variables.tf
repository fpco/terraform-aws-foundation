variable "name_prefix" {
  description = "Name to prefix various resources with"
}

variable "region" {
  description = "Region were VPC will be created"
}

variable "cidr" {
  description = "CIDR range of VPC. eg: 172.16.0.0/16"
}

variable "public_subnet_cidrs" {
  type        = "list"
  description = "A list of public subnet CIDRs to deploy inside the VPC."
}

variable "private_subnet_cidrs" {
  description = "A list of private subnet CIDRs to deploy inside the VPC. Should not be higher than public subnets count"
  default     = []
}

variable "azs" {
  type        = "list"
  description = "A list of Availaiblity Zones in the region"
}

variable "extra_tags" {
  description = "Extra tags that will be added to VPC, DHCP Options, Internet Gateway, Subnets and Routing Table."
  default     = {}
}

variable "enable_dns_hostnames" {
  default     = true
  description = "boolean, enable/disable VPC attribute, enable_dns_hostnames"
}

variable "enable_dns_support" {
  default     = true
  description = "boolean, enable/disable VPC attribute, enable_dns_support"
}

variable "dns_servers" {
  default     = ["AmazonProvidedDNS"]
  description = "list of DNS servers"
}
