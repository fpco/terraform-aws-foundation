variable "name_prefix" {
  default     = "dev"
  description = "Prefix that will be added to names of all resources"
  type        = string
}

variable "private_key" {
  description = "Contents of SSH private key for connecting to the instance."
  type        = string
}

variable "public_key" {
  description = "Contents of SSH public key for connecting to the instance."
  type        = string
}

variable "vpc_id" {
  description = "VPC, where vpn-gateway will be deployed in."
  type        = string
}

variable "vpc_cidr" {
  description = "VPC CIDR, ip range that vpn-gateway will accept traffic from."
  type        = string
}

variable "vpc_subnet_id" {
  description = "Subnet id where vpn-gateway should be deployed in."
  type        = string
}

variable "vpc_route_table_id" {
  description = "Route table, which will have the vpn-gateway added to."
  type        = string
}

variable "route53_zone_id" {
  description = "Private Host Zone id that will be associated with the VPC."
  type        = string
}

variable "vpn_cidr" {
  default     = "10.0.0.0/8"
  description = "Private network IP range."
  type        = string
}

variable "vpn_hostname" {
  default     = "connect.example.com"
  description = "VPN hostname."
  type        = string
}

variable "vpn_username" {
  description = "VPN username."
  type        = string
}

variable "vpn_password" {
  description = "VPN password."
  type        = string
}

variable "vpn_config_file" {
  default     = "/etc/vpn-gateway.conf"
  description = "VPN configuration file path."
  type        = string
}

variable "vpn_docker_image" {
  default     = "vpn-gateway"
  description = "Docker image."
  type        = string
}

variable "instance_type" {
  default     = "t2.small"
  description = "EC2 instance type."
  type        = string
}

