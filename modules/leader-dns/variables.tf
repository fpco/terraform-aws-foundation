variable "name" {
  default     = ""
  description = "Name of the cluster of nomad/consul leaders. foo in foo.domain"
  type        = string
}

variable "domain" {
  description = "domain to use when creating the leaders' record set, domain in foo.domain"
  type        = string
}

variable "vpc_id" {
  default     = ""
  description = "ID of the management cluster's AWS VPC to deploy to"
  type        = string
}

variable "cidr_a" {
  description = "CIDR IP block for leader subnet a, expected to be a /28"
  type        = string
}

variable "cidr_c" {
  description = "CIDR IP block for leader subnet c, expected to be a /28"
  type        = string
}

variable "ttl" {
  default     = "300"
  description = "TTL to set in aws_route53_record"
  type        = string
}

