variable "vpc_id" {
  description = "VPC ID where route tables will be placed in"
}

variable "name_prefix" {
  description = "Project name. It will be prepended to route tables."
}

variable "nat_count" {
  description = "How many NAT gateways to setup"
}

variable "public_subnet_ids" {
  description = "Public subnet IDs where to place the gateways"
  type        = "list"
}

variable "private_subnet_ids" {
  description = "Private subnet IDs for the route table associations, i.e subntes that will get internet acess"
  type        = "list"
}

variable "extra_tags" {
  default     = {}
  description = "Any extra tags to assign to route tables"
}
