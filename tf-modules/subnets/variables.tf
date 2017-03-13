variable "name_prefix" {
  description = "Project name. It will be prepended to all subnets"
}

variable "vpc_id" {
  description = "VPC ID where subnets will be created"
}

variable "public_subnet_cidrs" {
  description = "A list of public subnet CIDRs"
  default = []
}

variable "private_subnet_cidrs" {
  description = "A list of private subnet CIDRs. Should not be higher than public subnets count"
  default = []
}

variable "azs" {
  type = "list"
  description = "A list of Availaiblity Zones in the region"
}

variable "extra_tags" {
  description = "Extra tags that will be added to Subnets"
  default = {}
}
