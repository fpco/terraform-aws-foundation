variable "name_prefix" {}

variable "ami" {}

variable "instance_type" {
  default = "t2.micro"
}

variable "min_server_count" {
  description = "Minimum number of EC2 instances running Kibana"
  default     = 1
}

variable "max_server_count" {
  description = "Maximum number of EC2 instances running Kibana"
  default     = 1
}

variable "desired_server_count" {
  description = "Desired number of EC2 instances running Kibana"
  default     = 1
}

variable "vpc_id" {
  description = "VPC id where Kibana servers should be deployed in"
}

variable "private_subnet_ids" {
  description = "A list of private subnet ids to deploy EC2 instances running Kibana in"
  type        = "list"
}

variable "public_subnet_ids" {
  description = "A list of public subnet ids to deploy ELB in. Unused when separately deployed AALB is being used."
  default     = []
  type        = "list"
}

variable "elasticsearch_url" {
  description = "Elasticsearch endpoint URL"
}

variable "key_name" {
  description = "SSH key name to use for connecting to all nodes"
}

variable "extra_sg_ids" {
  default     = []
  description = "Extra Security Group IDs that will be added to all instances running Kibana. This is a way to add extra services, SSH access for instance."
}

variable "credstash_install_snippet" {
  description = "Ubuntu bash script snippet for installing credstash and its dependencies"
}

variable "credstash_get_cmd" {
  description = "Credstash get command with region and table values set."
}

variable "alb" {
  type = "map"

  description = <<DOC
Information on already existing Application LB or settings for a Classic LB to
be deployed:
  * certificate_arn (required) - ARN of ACM certificate to be used.
  * security_group_id (required)
  * deploy_elb (optional, default: false) - if set to true ELB will be deployed
    and used instead of an ALB.
  * deploy_elb_internal (optional, default: true) - if set to false ELB will be
    deployed as an external one.
  * deploy_elb_cross_zone (optional, default: true) - if set to false cross AZ
    for ELB will be off.
Below are only required if `deploy_elb` is set to `false` or omitted:
  * arn - ALB ARN.
  * dns_name - host-header for ALB listener rule.
  * zone_id - not used directly, but will be returned in the `lb` output map.
DOC
}

variable "kibana_version" {
  description = "Which version of Kibana to install"
}
