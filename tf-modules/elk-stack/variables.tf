variable "name_prefix" {
  default = "elk-dev"
  description = "Prefix that will be added to names of all resources"
}

variable "region" {
  description = "Region to deploy ELK stack in"
}

variable "vpc_id" {
  description = "VPC ID for the ELK stack"
}

variable "public_subnet_ids" {
  type = "list"
  description = "Public subnet ids, where Kibana and Logstash ELBs will be placed"
}

variable "private_subnet_ids" {
  type = "list"
  description = "Private subnet ids, where Kibana, Logstash and ELasticsearch instances will be placed. This is also the order in which nodes will be deployed in."
}

variable "public_cidrs" {
  default = []
  description = "List of CIDRs that will have access to Kibana UI and SSH to EC2 instances"
}

variable "elasticsearch_dns_name" {
  description = "DNS name for Elasticsearch"
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

variable "elasticsearch_index_retention_period" {
  default = 60
  description = "Age of Elasticsearch indices in days before they will be considered old and be pruned by the curator"
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
variable "logstash_extra_cidrs" {
  description = "Extar CIDRs that will be allowed to connect to Logstash over Beat protocol"
  default = []
}

variable "route53_zone_id" {
  description = "Route53 Zone id where ELB should get added a record to"
}

variable "kibana_username" {
  default = "kibanaadmin"
  description = "Username for Kibana authentication"
}

variable "kibana_password" {
  default = "kibanaadmin"
  description = "Password for Kibana authentication"
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

variable "certstrap_ca_force_new" {
  default = false
  description = "New CA certificate will be created even if there already one exitsts with the same CN"
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

variable "ssh_pubkey" {
  default = "ssh_key.pub"
  description = "Path to the SSH public key file to use for connecting to all instances. Ignored if ssh_keyname is non-empty"
}

variable "ssh_key_name" {
  default = ""
  description = "Use an existing ssh key pair, must already be created on AWS. If empty, ssh_pubkey is used to create a new pair with name 'var.name_prefix + key'"
}

variable "ssh_allow" {
  default = true
  description = "Allow SSH access to EC2 instances."
}

