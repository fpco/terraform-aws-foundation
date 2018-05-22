variable "name_prefix" {
  description = "Prefix that will be added to names of all resources"
}

variable "name_suffix" {
  description = "suffix to include when naming the various resources"
  default     = "kube-worker"
}

variable "vpc_id" {
  description = "VPC id for the security group"
}

variable "cidr_blocks_ssh" {
  description = "list of CIDR blocks with access to SSH on the workers"
  type        = "list"
}

variable "cidr_blocks_open_ingress" {
  description = "list of CIDR blocks with access to all ports on the workers"
  type        = "list"
}
