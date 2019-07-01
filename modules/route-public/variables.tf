variable "name_prefix" {
  description = "Name to prefix various resources with"
  type        = string
}

variable "vpc_id" {
  description = "VPC ID where subnets will be created"
  type        = string
}

variable "extra_tags" {
  type        = map(string)
  default     = {}
  description = "Extra tags that will be added to VPC, DHCP Options, Internet Gateway, Subnets and Routing Table."
}

variable "public_subnet_ids" {
  type        = list(string)
  description = "The list of public subnet IDs to route thru the public gateway"
}

