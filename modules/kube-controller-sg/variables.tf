variable "name_prefix" {
  description = "Prefix that will be added to names of all resources"
}

variable "name_suffix" {
  description = "suffix to use when naming the various resources"
  default     = "kube-controller"
}

variable "vpc_id" {
  description = "VPC id for the security group"
}

variable "api_port" {
  description = "TCP port the kube controller API is listening on"
  default     = "6443"
}

variable "cidr_blocks_api" {
  description = "list of CIDR blocks that should have access to the kube API"
  type        = "list"
}

variable "cidr_blocks_ssh" {
  description = "list of CIDR blocks that should have access to SSH"
  type        = "list"
}

variable "cidr_blocks_etcd" {
  description = "list of CIDR blocks that should have access to etcd"
  type        = "list"
}
