variable "aws_region" {
  description = "AWS region this env is deployed to"
}

variable "name" {
  description = "name of this deployment, the VPC and all resources therein"
}

variable "aws_availability_zones" {
  description = "List of availability zones to use. Should match number of CIDR blocks"
}

variable "kubernetes_cluster_name" {
  default     = "kops-vpc.dev-sandbox.fpcomplete.com"
  description = "name of the kube cluster deployed to the VPC, used to tag resources"
}

variable "vpc_cidr" {
  description = "Top-level CIDR for the whole VPC network space"
  default     = "10.120.0.0/16"
}

variable "public_subnet_cidrs" {
  description = "list of CIDR ranges for the public subnets"
  default     = ["10.120.10.0/24", "10.120.11.0/24", "10.120.12.0/24"]
}

variable "extra_tags" {
  description = "Extra tags that will be added to aws_subnet resources"
  default     = {}
}

variable "cluster_admin_keyname" {
  description = "Admin SSH keyname that was imported to AWS"
}
