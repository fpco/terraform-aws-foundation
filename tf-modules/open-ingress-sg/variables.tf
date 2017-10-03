variable "name_prefix" {
  description = "The name prefix for this security group"
  type        = "string"
}

variable "name_suffix" {
  description = "The suffix to append to `name_prefix`"
  default     = "open-ingress"
}

variable "vpc_id" {
  description = "The ID of the VPC to deploy to"
  type        = "string"
}

variable "cidr_blocks" {
  description = "CIDR blocks to use with `ingress`"
  type        = "list"
}
