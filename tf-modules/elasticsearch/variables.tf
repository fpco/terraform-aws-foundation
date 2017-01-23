variable "region" {
}

variable "name_prefix" {
  default = "dev"
}

variable "node_ami" {
  description = "AMI to use for all nodes. Should be Ubuntu based with apt-get installed"
}

variable "master_node_count" {
  description = "Number of master nodes in the cluster. It should be either 1 or 3, in order to prevent split brain."
  default = 1
}

variable "data_node_count" {
  description = "Number of data nodes in the cluster."
  default = 1
}

variable "master_node_instance_type" {
  default = "t2.small"
}

variable "data_node_instance_type" {
  description = "Data nodes EC2 instance type"
  default = "t2.small"
}

variable "master_node_snapshot_ids" {
  type = "list"
  default = []
}

variable "data_node_snapshot_ids" {
  type = "list"
  default = []
}

variable "master_node_ebs_size" {
  description = "Size of the data volume for a master node"
  default = 4
}

variable "data_node_ebs_size" {
  description = "Size of the data volume for a master node"
  default = 16
}

variable "vpc_id" {
}

variable "vpc_azs" {
  description = "A list of availability zones to deploy subnets in"
  default     = ["us-east-1a", "us-east-1d", "us-east-1c"]
}

variable "vpc_private_subnet_cidrs" {
  description = "The CIDR ranges for the VPC's private subnets"
  default     = ["172.16.0.0/24", "172.16.1.0/24", "172.16.2.0/24"]
}

variable "vpc_public_subnet_cidrs" {
  description = "The CIDR ranges for the VPC's public subnets"
  default     = ["172.16.4.0/24", "172.16.5.0/24", "172.16.6.0/24"]
}

variable "vpc_private_subnet_ids" {
  type = "list"
  description = "IDs of private subnets."
}

variable "route53_zone_id" {
  description = "Route53 Zone id where ELB should get added a record to"
}

variable "elasticsearch_dns_name" {
  description = "DNS name for Elasticsearch"
  default = "elasticsearch-dev.e1c.net"
}

variable "key_name" {
  description = "SSH key name to use for connecting to all nodes."
}
