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

variable "vpc_id" {
  description = "VPC ID for the ELK stack"
}

variable "vpc_route_table_id" {
  description = "ID of the route table, with which created subnets will be associated with"
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
  default = "t2.micro"
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
  default = "t2.micro"
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
  default = "t2.micro"
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

variable "certstrap_depot_path" {
  default = ""
  description = "Local path, where generated SSL certifcates will be stored in. Certificates will be removed from local file system if left empty and will only be retained in credential store"
}

variable "certstrap_ca_common_name" {
  default = "Logstash"
  description = "Common Name to be used during CA certificate generation"
}

variable "certstrap_ca_passphrase" {
  default = ""
  description = "Passphrase for SSL Key encryption to be used during CA certificate generation"
}

variable "credstash_table_name" {
  default = "credential-store"
  description = "DynamoDB table used by credstash to store credentials"
}

variable "credstash_kms_key_arn" {
  description = "Master KMS key ARN for getting SSL server key using credstash"
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
