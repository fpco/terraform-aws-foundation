variable "name_prefix" {
  default = "dev"
  description = "Prefix that will be added to names of all resources"
}

variable "vpc_id" {
  description = "VPC ID where Kibana ALB will be placed"
}

variable "subnet_ids" {
  description = "A list of subnet ids to attach to ALB"
  type = "list"
}

variable "internal" {
  default = true
  description = "Should this ALB be internal or not."
}

variable "extra_app_names" {
  default = ""
  description = "Names of other apps this ALB will be used for, eg. '+Elasticsearch'"
}

variable "kibana_dns_name" {
  description = "DNS name for Kibana endpoint. For SSL Certificate in ACM, if different, set 'kibana_dns_ssl_name'"
}

variable "kibana_dns_ssl_name" {
  default = ""
  description = "DNS name for Kibana endpoint SSL. An SSL certificate is expected to be present in ACM for this domain. If left empty 'kibana_dns_name' will be checked instead."
}

variable "user_ingress_cidrs" {
  type = "list"
  description = "List of CIDRs that will have access to Kibana UI"
}

variable "http_target_group_arn" {
  description = "Target group for HTTP traffic"
}

variable "https_target_group_arn" {
  description = "Target group for HTTPS traffic"
}
