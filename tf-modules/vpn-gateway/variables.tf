variable "name_prefix" {
  default = "dev"
  description = "Prefix that will be added to names of all resources"
}

variable "private_key" {
  description = "Contents of SSH private key for connecting to the instance."
}

variable "public_key" {
  description = "Contents of SSH public key for connecting to the instance."
}

variable "vpc_id" {
  description = "VPC, where vpn-gateway will be deployed in."
}

variable "vpc_cidr" {
  description = "VPC CIDR, ip range that vpn-gateway will accept traffic from."
}

variable "vpc_subnet_id" {
  description = "Subnet id where vpn-gateway should be deployed in."
}


variable "vpc_route_table_id" {
  description = "Route table, which will have the vpn-gateway added to."
}

variable "route53_zone_id" {
  description = "Private Host Zone id that will be associated with the VPC."
}

variable "vpn_cidr" {
  default = "10.0.0.0/8"
  description = "Private network IP range."
}

variable "vpn_hostname" {
  default = "connect.example.com"
  description = "VPN hostname."
}

variable "vpn_username" {
  description = "VPN username."
}

variable "vpn_password" {
  description = "VPN password."
}

variable "vpn_config_file" {
  default = "/etc/vpn-gateway.conf"
  description = "VPN configuration file path."
}

variable "vpn_docker_image" {
  default = "vpn-gateway"
  description = "Docker image."
}

variable "instance_type" {
  default = "mt2.small"
  description = "EC2 instance type."
}

