variable "project" {
  type = string
}

variable "env" {
  type = string
}

variable "region" {
  type = string
}

variable "cidr" {
  description = "CIDR range of VPC. eg: 172.16.0.0/16"
  type = string
}

variable "public_subnets" {
  type        = list(string)
  description = "A list of public subnet cidrs to deploy inside the VPC."
}

variable "azs" {
  type        = list(string)
  description = "A list of Availaiblity zones in the region"
}

variable "kube_fqdn" {
  type = string
}

