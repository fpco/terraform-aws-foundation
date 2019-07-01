variable "name_prefix" {
  description = "name of this deployment, the VPC and all resources therein"
}

variable "region" {
  description = "AWS region this env is deployed to"
}

variable "availability_zones" {
  description = "List of availability zones to use. Should match number of CIDR blocks"
}

variable "kubernetes_cluster_name" {
  description = "name of the kube cluster deployed to the VPC, used to tag resources"
}

variable "vpc_cidr" {
  description = "Top-level CIDR for the whole VPC network space"
}

variable "public_subnet_cidrs" {
  description = "list of CIDR ranges for the public subnets"
}

variable "extra_tags" {
  description = "Extra tags that will be added to aws_subnet resources"
  default     = {}
}

variable "cluster_admin_keyname" {
  description = "Admin SSH keyname that was imported to AWS"
}

