variable "name_prefix" {
  default     = "dev"
  description = "Prefix that will be added to names of all resources"
  type        = string
}

variable "vpc_id" {
  description = "VPC ID where Web Server ALB will be placed"
  type        = string
}

variable "subnet_ids" {
  description = "A list of subnet ids to attach ALB to"
  type        = list(string)
}

variable "internal" {
  default     = true
  description = "Should this ALB be internal or not."
  type        = string
}

variable "app_names" {
  default     = ""
  description = "Names of other apps this ALB will be used for, eg. 'Kibana+Elasticsearch'"
  type        = string
}

variable "dns_name" {
  description = "DNS name for Web Server endpoint. For SSL Certificate in ACM, if different, set 'dns_ssl_name'"
  type        = string
}

variable "dns_ssl_name" {
  default     = ""
  description = "DNS name for Web Server endpoint SSL. An SSL certificate is expected to be present in ACM for this domain. If left empty 'dns_name' will be checked instead."
  type        = string
}

variable "user_ingress_cidrs" {
  default     = []
  description = "Optional list of CIDRs that will have access to ALBs ports 80 and 443."
  type        = list(string)
}

variable "http_target_group_arn" {
  description = "Target group for HTTP traffic"
  type        = string
}

variable "https_target_group_arn" {
  description = "Target group for HTTPS traffic"
  type        = string
}

