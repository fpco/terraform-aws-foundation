variable "name_prefix" {
  description = "Name to prefix various resources with"
}

variable "vpc_id" {
  description = "VPC ID where subnets will be created"
}

variable "extra_tags" {
  type        = "map"
  default     = {}
  description = "Extra tags that will be added to VPC, DHCP Options, Internet Gateway, Subnets and Routing Table."
}

variable "public_subnet_ids" {
  type        = "list"
  description = "The list of public subnet IDs to route thru the public gateway"
}
