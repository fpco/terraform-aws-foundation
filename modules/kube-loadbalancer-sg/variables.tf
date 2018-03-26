variable "name" {
  description = "Name of the Kube Controller security group"
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
