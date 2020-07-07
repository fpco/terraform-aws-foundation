variable "extra_tags" {
  description = "Extra tags that will be added to aws_subnet resources"
  default     = {}
}

variable "lifecycle_name_prefix" {
  description = "Prefix used for resource names."
  default     = "lifecycled-eg"
}

variable "name_prefix" {
  description = "Prefix used for resource names."
  default     = "asg-lc"
}

variable "name" {
  description = "name of the project, use as prefix to names of resources created"
  default     = "test-lifecycle-project"
}

variable "region" {
  description = "Region where the project will be deployed"
  default     = "us-east-2"
}

variable "vpc_cidr" {
  description = "Top-level CIDR for the whole VPC network space"
  default     = "10.23.0.0/16"
}

variable "ssh_pubkey" {
  description = "File path to SSH public key"
  default     = "./id_rsa.pub"
}

variable "ssh_key" {
  description = "File path to SSH public key"
  default     = "./id_rsa"
}

variable "public_subnet_cidrs" {
  default     = ["10.23.11.0/24", "10.23.12.0/24", "10.23.13.0/24"]
  description = "A list of public subnet CIDRs to deploy inside the VPC"
}

variable "tags" {
  description = "A map of tags (key-value pairs) passed to resources."
  type        = map(string)
  default     = {}
}

