variable "name" {
  description = "Name of this deployment, the VPC and all resources therein"
  default     = "kube-stack-test"
}

variable "name_suffix" {
  description = "suffix to include when naming the various resources"
}

variable "name_prefix" {
  description = "Prefix that will be added to names of all resources"
}

variable "vpc_id" {
  description = "VPC id where Elasticsearch cluster is deployed"
}

variable "vpc_cidr" {
  description = "Top-level CIDR for the whole VPC network space"
}

variable "vpc_cidr_controller_api" {
  description = "Top-level CIDR for kube api access"
}

variable "vpc_cidr_controller_ssh" {
  description = "Top-level CIDR for ssh access"
}

variable "vpc_cidr_controller_etcd" {
  description = "Top-level CIDR for etcd"
}
