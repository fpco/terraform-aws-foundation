variable "project" {}

variable "env" {}

variable "region" {}

variable "cidr" {
  description = "CIDR range of VPC. eg: 172.16.0.0/16"
}

variable "public_subnets" {
  type = "list"
  description = "A list of public subnet cidrs to deploy inside the VPC."
}

variable "azs" {
  type = "list"
  description = "A list of Availaiblity zones in the region"
}

variable "kube_fqdn" {}
