variable "name_prefix" {
  description = "Prefix that will be added to names of all resources"
  type        = string
}

variable "name_suffix" {
  description = "suffix to use when naming the various resources"
  default     = "kube-controller"
  type        = string
}

variable "vpc_id" {
  description = "VPC id for the security group"
  type        = string
}

variable "api_port" {
  description = "TCP port the kube controller API is listening on"
  default     = "6443"
  type        = string
}

variable "cidr_blocks_api" {
  description = "list of CIDR blocks that should have access to the kube API"
  type        = list(string)
}

variable "cidr_blocks_ssh" {
  description = "list of CIDR blocks that should have access to SSH"
  type        = list(string)
}

variable "cidr_blocks_etcd" {
  description = "list of CIDR blocks that should have access to etcd"
  type        = list(string)
}

