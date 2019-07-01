variable "region" {
  default = "us-east-2"
}

# VPC
variable "name_prefix" {
  description = "The name of the environment together with the project name."
  default     = "dev"
}

variable "vpc_azs" {
  description = "A list of availability zones to deploy subnets in"
  type = list(string)
}

variable "vpc_cidr" {
  description = "The CIDR range of the VPC"
}

variable "vpc_public_subnet_cidrs" {
  description = "The CIDR ranges for the VPC's public subnets"
  type = list(string)
}

variable "pub_key_file" {
  default     = "ssh_key.pub"
  description = "Path to the SSH public key file to use for connecting to the instance."
}

variable "priv_key_file" {
  default     = "ssh_key"
  description = "Path to the SSH private key file to use for connecting to the instance."
}

# VPN Gateway
variable "route53_zone_id" {
  description = "The Zone ID to use for VPN DNS discovery"
}

variable "vpn_cidr" {
  description = "Private network IP range."
}

variable "vpn_hostname" {
  description = "VPN hostname."
}

variable "vpn_username" {
  description = "VPN username."
}

variable "vpn_password" {
  description = "VPN password."
}

