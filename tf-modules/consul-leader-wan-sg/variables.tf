variable "name" {
  description = "A short identifier to use in the name of the security group"
}

variable "vpc_id" {
  description = "The ID of the VPC to associate the security group with"
}

variable "cidr_blocks" {
  description = "The list of CIDR IP blocks allowed to access the consul WAN port"
  type        = "list"
}

variable "wan_port" {
  default     = "8302"
  description = "The port to use for consul WAN"
}
