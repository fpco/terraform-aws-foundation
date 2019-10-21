variable "extra_tags" {
  description = "Extra tags that will be added to aws_subnet resources"
  default     = {}
}

variable "name" {
  description = "name of the project, use as prefix to names of resources created"
  default     = "test-ad-project"
}

variable "vpc1_cidr" {
  description = "Top-level CIDR for the whole VPC 1 network space"
  default     = "10.23.0.0/16"
}

variable "vpc2_cidr" {
  description = "Top-level CIDR for the whole VPC 2 network space"
  default     = "10.24.0.0/16"
}

variable "ssh_pubkey" {
  description = "File path to SSH public key"
  default     = "./id_rsa.pub"
}

variable "ssh_key" {
  description = "File path to SSH public key"
  default     = "./id_rsa"
}

variable "public_subnet_cidrs_vpc1" {
  default     = ["10.23.20.0/24"]
  description = "A list of public subnet CIDRs to deploy inside the VPC 1"
}

variable "private_subnet_cidrs_vpc1" {
  default     = ["10.23.21.0/24", "10.23.22.0/24"]
  description = "A list of private subnet CIDRs to deploy inside the VPC 1"
}

variable "public_subnet_cidrs_vpc2" {
  default     = ["10.24.0.0/24"]
  description = "A list of public subnet CIDRs to deploy inside the VPC 1"
}

variable "admin_password" {
  type = string
}

variable "active_directory_password" {
  type = string
}

variable "private_subnet_extra_tags" {
  description = "Extra tags that will be added to private subnets."
  default     = {}
  type        = map(string)
}

variable "nat_gateway_extra_tags" {
  description = "Extra tags that will be added to NAT gateway and routing tables for the private subnets"
  default     = {}
  type        = map(string)
}
