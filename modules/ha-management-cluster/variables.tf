variable "name" {
  default     = ""
  description = "Name of the management cluster"
  type = string
}

variable "vpc_id" {
  default     = ""
  description = "ID of the management cluster's AWS VPC to deploy to"
  type = string
}

variable "route_table_id" {
  default     = ""
  description = "ID of the routing table to hook up the cluster network to"
  type = string
}

variable "worker_cidr_blocks" {
  description = "List of CIDR IP blocks with access to the management services (string-list)"
  type = string
}

variable "consul_secret_key" {
  default     = ""
  description = ""
  type = string
}

variable "consul_client_token" {
  default     = ""
  description = "Token for consul client access, provided to consul agent as salt pillar"
  type = string
}

variable "consul_leader_dns" {
  default     = ""
  description = "DNS to consul leaders, provided to consul agent as salt pillar"
  type = string
}

variable "region" {
  default     = ""
  description = "The AWS region, provided to consul as datacenter"
  type = string
}

variable "cidr_a" {
  description = "The CIDR block for subnet a"
  type = string
}

variable "cidr_c" {
  description = "The CIDR block for subnet c"
  type = string
}

variable "key_name" {
  description = "The name of the (AWS) SSH key to associate with the instance"
  type = string
}

variable "instance_type" {
  default     = "t2.medium"
  description = "The base AMI for each AWS instance created"
  type = string
}

variable "ami" {
  description = "The base AMI for each AWS instance created"
  type = string
}

variable "public_ip" {
  default     = true
  description = "Boolean flag to enable/disable `map_public_ip_on_launch` in each `aws_subnet`"
  type = bool
}

variable "security_group_ids" {
  description = "String-list of security group ids to associate with the cluster"
  type = string
}

variable "root_volume_type" {
  default     = "gp2"
  description = "The type of EBS volume to use for the root block device"
  type = string
}

variable "root_volume_size" {
  default     = "15"
  description = "The size of the EBS volume (in GB) for the root block device"
  type = number
}

