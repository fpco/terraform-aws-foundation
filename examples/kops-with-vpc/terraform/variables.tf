# General vars

variable "region" {
  default     = "us-east-2"
  description = "AWS region this env is deployed to"
}

variable "name" {
  default     = "prod"
  description = "name of this deployment, the VPC and all resources therein"
}

variable "aws_availability_zones" {
  default     = ["us-east-2a", "us-east-2b", "us-east-2c"]
  description = "List of availability zones to use. Should match number of CIDR blocks"
}

variable "kubernetes_cluster_name" {
  default     = "mykube.dev-sandbox.fpcomplete.com"
  description = "name of the kube cluster deployed to the VPC, used to tag resources"
}

/* IMPORTANT :

Make sure to use a unique CIDR block for each VPC. It just helps
avoid confusion later on.

*/

variable "vpc_cidr" {
  description = "Top-level CIDR for the whole VPC network space"
  default     = "10.120.0.0/16"
}

/* IMPORTANT :

Make sure that these are unique in the VPC. If you are provisioning just new
subnets in an existing VPC (with the above vpc_cidr) and the below block is already used
for some other cluster you can update this to something like

  default     = ["10.120.20.0/24", "10.120.21.0/24", "10.120.22.0/24"]

*/
variable "kube_public_subnet_cidrs" {
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
