variable "extra_tags" {
  description = "Extra tags that will be added to aws_subnet resources"
  default     = {}
}

variable "name" {
  description = "name of the project, use as prefix to names of resources created"
  default     = "pm-vpc-3"
}

variable "region" {
  description = "Region where the project will be deployed"
  default     = "eu-central-1"
}

variable "vpc_cidr" {
  description = "Top-level CIDR for the whole VPC network space"
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

variable "public_subnet_cidrs" {
  default     = ["10.24.11.0/24", "10.24.12.0/24", "10.24.13.0/24"]
  description = "A list of public subnet CIDRs to deploy inside the VPC"
}

variable "private_subnet_cidrs" {
  default     = ["10.24.21.0/24", "10.24.22.0/24", "10.24.23.0/24"]
  description = "A list of private subnet CIDRs to deploy inside the VPC"
}

variable "vpn_remote_ip" {
  default     = "change-me"
  description = "IP address of the remote VPN for AWS to associate with"
}

variable "vpn_static_routes" {
  default     = ["0.0.0.0/0"]
  description = "list of static routes to use with AWS customer gateway (VPN)"
}

