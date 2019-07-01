variable "name_prefix" {
  default = "dev"
  type    = string
}

variable "node_ami" {
  description = "AMI to use for all nodes. Should be Ubuntu based with apt-get installed"
  type        = string
}

variable "master_node_count" {
  description = "Number of master nodes in the cluster. It should be either 1 or 3, in order to prevent split brain."
  default     = 1
  type        = string
}

variable "data_node_count" {
  description = "Number of data nodes in the cluster."
  default     = 1
  type        = string
}

variable "master_node_instance_type" {
  description = "Master nodes EC2 instance type"
  default     = "t2.micro"
  type        = string
}

variable "data_node_instance_type" {
  description = "Data nodes EC2 instance type"
  default     = "t2.micro"
  type        = string
}

variable "master_node_snapshot_ids" {
  type    = list(string)
  default = []
}

variable "data_node_snapshot_ids" {
  type    = list(string)
  default = []
}

variable "master_node_ebs_size" {
  description = "Size of the data volume for a master node"
  default     = 4
  type        = string
}

variable "data_node_ebs_size" {
  description = "Size of the data volume for a master node"
  default     = 16
  type        = string
}

variable "vpc_id" {
  description = "VPC id where Elasticsearch cluster is deployed"
  type        = string
}

variable "private_subnet_ids" {
  type        = list(string)
  description = "Private subnet ids where instances will be deployed in."
}

variable "public_subnet_ids" {
  type        = list(string)
  description = "Public subnet ids where ELB will be deployed in. Pass private subnet ids, if you expect elasticsearch to be a private resource only."
}

variable "extra_sg_ids" {
  default     = []
  description = "Extra Security Group IDs that will be added to all Elasticvsearch nodes. This is a way to add extra services, such as SSH access for instance."
  type        = list(string)
}

variable "extra_elb_sg_ids" {
  default     = []
  description = "Extra Security Group IDs that will be added to Elasticsearch API Load Balancer"
  type        = list(string)
}

variable "extra_setup_snippet" {
  default     = ""
  description = "Extra snippet to run after Elasticsearch has been installed and configured"
  type        = string
}

variable "extra_config" {
  default     = ""
  description = "Extra elasticsearch configuration in yaml format that will be applied to all nodes"
  type        = string
}

variable "elasticsearch_dns_name" {
  description = "DNS name for Elasticsearch"
  type        = string
}

variable "key_name" {
  description = "SSH key name to use for connecting to all nodes"
  type        = string
}

variable "deploy_curator" {
  default     = false
  description = "Should a curator be installed and enabled on all master eligible nodes. If enabled it will only run on currently elected master."
  type        = string
}

variable "index_retention_period" {
  default     = 60
  description = "Age of Elasticsearch indices in days before they will be considered old and be pruned by the curator. Set to 0 in order to disable."
  type        = string
}

variable "extra_curator_actions" {
  default     = ""
  description = "YAML formatted dictionary of actions, as described in documentation, but started at index '2', since action number '1' is the one that purges old indices."
  type        = string
}

variable "logstash_beats_address" {
  description = "DNS name and port of where logstash is listenting with beats protocol."
  type        = string
}

variable "credstash_kms_key_arn" {
  description = "Master KMS key ARN for getting SSL server key using credstash"
  type        = string
}

variable "credstash_reader_policy_arn" {
  description = "Secrets Reader Policy ARN that was created by 'credstash-setup' module. Reading will be disabled if not supplied."
  type        = string
}

variable "credstash_install_snippet" {
  description = "Ubuntu bash script snippet for installing credstash and its dependencies"
  type        = string
}

variable "credstash_get_cmd" {
  description = "Credstash get command with region and table values set."
  type        = string
}

variable "internal_alb" {
  type = map(string)

  description = <<DOC
Information on already existing Application LB or settings for a Classic LB to
be deployed:
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

variable "external_alb_setup" {
  default = false
  description = "Should an external access be allowed to ES API on port 9201 with HTTPS and BasicAuth."
  type = string
}

variable "external_alb" {
  type = map(string)
  default = {}

  description = <<DOC
 This variable is unused, unless `external_alb_setup` is set to true. Information on already
 existing Application LB or settings for a Classic LB to be deployed:
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

variable "external_alb_ingress_cidrs" {
default     = []
description = "CIDRs that are allowed to access Elasticsearch API over HTTPS on port 9201 with BasicAuth."
type        = list(string)
}

variable "elasticsearch_version" {
description = "Which version of elasticsearch to install"
type        = string
}

