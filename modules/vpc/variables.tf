variable "name_prefix" {
  description = "Name to prefix the VPC/DHCP resources with"
  type        = string
}

variable "region" {
  description = "Region were VPC will be created"
  type        = string
}

variable "cidr" {
  description = "CIDR range of VPC. eg: 172.16.0.0/16"
  type        = string
}

variable "extra_tags" {
  description = "Extra tags that will be added to VPC and DHCP resources"
  default     = {}
  type        = map(string)
}

variable "enable_dns_hostnames" {
  default     = true
  description = "boolean, enable/disable VPC attribute, enable_dns_hostnames"
  type        = string
}

variable "enable_dns_support" {
  default     = true
  description = "boolean, enable/disable VPC attribute, enable_dns_support"
  type        = string
}

variable "dns_servers" {
  default     = ["AmazonProvidedDNS"]
  description = "list of DNS servers for the DHCP options resource"
  type        = list(string)
}

variable "ntp_servers" {
  default     = []
  description = "list of NTP servers for the DHCP options resource"
  type        = list(string)
}

variable "domain_name" {
  default     = ""
  description = "string, domain name for the DHCP options resource"
  type        = string
}
