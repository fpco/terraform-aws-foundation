variable "access_key" {}

variable "secret_key" {}

variable "token" {}

variable "name_prefix" {
  default = "elk-dev"
  description = "Prefix that will be added to names of all resources"
}

variable "region" {
  default = "us-east-1"
  description = "Region to deploy ELK stack in"
}

variable "vpc_cidr" {
  description = "The CIDR range of the VPC"
  default     = "172.16.0.0/21"
}

variable "vpc_azs" {
  description = "A list of availability zones. This is also the order in which nodes will be deployed in"
  default     = ["us-east-1a", "us-east-1c", "us-east-1d"]
}

variable "vpc_private_subnet_cidrs" {
  description = "The CIDR ranges for the VPC's private subnets"
  default     = ["172.16.0.0/24", "172.16.1.0/24", "172.16.2.0/24"]
}

variable "vpc_public_subnet_cidrs" {
  description = "The CIDR ranges for the VPC's public subnets"
  default     = ["172.16.4.0/24", "172.16.5.0/24", "172.16.6.0/24"]
}


variable "elasticsearch_master_node_count" {
  description = "Number of master nodes in the cluster. It should be either 1, 3 or more, in order to deal with split brain"
  default = 1
}

variable "elasticsearch_master_node_instance_type" {
  default = "t2.small"
  description = "Instance type to use for master nodes."
}

variable "elasticsearch_master_node_ebs_size" {
  description = "Size of the data volume for a master node"
  default = 4
}

variable "elasticsearch_master_node_snapshot_ids" {
  default = [""]
  description = "List of snapshots ids to use for master nodes."
}

variable "elasticsearch_data_node_count" {
  description = "Number of data nodes in the cluster"
  default = 1
}

variable "elasticsearch_data_node_instance_type" {
  default = "t2.small"
  description = "Instance type to use for data nodes."
}

variable "elasticsearch_data_node_ebs_size" {
  description = "Size of the data volume for a master node"
  default = 16
}

variable "elasticsearch_data_node_snapshot_ids" {
  default = [""]
  description = "List of snapshots ids to use for data nodes."
}

variable "logstash_kibana_instance_type" {
  default = "t2.small"
  description = "Instance type to use for servers running Kibana+Logstash."
}

variable "logstash_kibana_min_server_count" {
  description = "Minimum number of EC2 instances running Logstash+Kibana"
  default = 1
}

variable "logstash_kibana_max_server_count" {
  description = "Maximum number of EC2 instances running Logstash+Kibana"
  default = 1
}

variable "logstash_kibana_desired_server_count" {
  description = "Desired number of EC2 instances running Logstash+Kibana"
  default = 1
}

variable "route53_zone_id" {
  description = "Route53 Zone id where ELB should get added a record to"
}

variable "kibana_dns_name" {
  description = "DNS name for Kibana"
}

variable "logstash_dns_name" {
  description = "DNS name for Logstash"
}

variable "logstash_ca_cert" {
  description = "CA certificate file path. Configures Logstash to trust clients with certificates signed by this CA"
}

variable "logstash_server_cert" {
  description = "Path to certificate that Logstash will use to authenticate with the client"
}

variable "logstash_server_key" {
  description = "Path to key that Logstash will use to authenticate with the client"
}

variable "priv_key_file" {
  default = "ssh_key"
  description = "Path to the SSH private key file to use for connecting to all instances"
}

variable "pub_key_file" {
  default = "ssh_key.pub"
  description = "Path to the SSH public key file to use for connecting to all instances"
}

variable "deploy_control_instance" {
  default = 1
  description = "Deploy EC2 instance, which can further be used to connect to all private instances. 1 (deploy) or 0 (don't deploy)"
}
