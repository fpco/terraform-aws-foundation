variable "name" {
  default     = ""
  description = "Name of the cluster of nomad/consul leaders. foo in foo.domain"
}

variable "domain" {
  description = "domain to use when creating the leaders' record set, domain in foo.domain"
}

variable "vpc_id" {
  default     = ""
  description = "ID of the management cluster's AWS VPC to deploy to"
}

variable "cidr_a" {
  description = "CIDR IP block for leader subnet a, expected to be a /28"
}

variable "cidr_c" {
  description = "CIDR IP block for leader subnet c, expected to be a /28"
}

variable "ttl" {
  default     = "300"
  description = "TTL to set in aws_route53_record"
}
