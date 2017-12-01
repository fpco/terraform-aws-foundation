variable "name_prefix" {
  description = "Project name. It will be prepended to all resources."
}

variable "region" {
  description = "Region were VPC will be created"
}

variable "cidr" {
  description = "CIDR range of VPC. eg: 172.16.0.0/16"
}

variable "dns_server_list" {
  description = "list of IPs to use as DNS servers (in DHCP options for the VPC)"
  default     = ["AmazonProvidedDNS"]
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

variable "nat_count" {
  description = "Number of NAT gateways to deploy. Should not be higher than the number of private subnets count"
  default     = 0
}
