variable "name_prefix" {
  description = "Name to prefix subnets with"
}

variable "vpc_id" {
  description = "VPC ID where subnets will be created"
}

variable "cidr_blocks" {
  type        = "list"
  description = "A list of CIDR blocks for the subnets"
}

variable "azs" {
  type        = "list"
  description = "A list of Availaiblity Zones to create subnets in"
}

variable "extra_tags" {
  description = "Extra tags that will be added to aws_subnet resources"
  default     = {}
}

// default to creating public subnets
variable "public" {
  description = "Boolean, maps to the map_public_ip_on_launch variable"
  default     = true
}
