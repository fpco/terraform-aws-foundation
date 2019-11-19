variable "internal" {
  default     = true
  type        = bool
  description = "Whether the LB should face Internet."
}

variable "subnet_ids" {
  type        = list(string)
  description = "The subnets for LBs to live in."
}

variable "vpc_id" {
  type        = string
  description = "The identifier of the VPC in which to create the target groups."
}

variable "tags" {
  type        = map(string)
  default     = {}
  description = "Tags for aws_lb resource."
}

variable "name_prefix" {
  type = string
}
