variable "name_prefix" {
  default     = "elk-dev"
  description = "Prefix that will be added to names of all resources"
}

variable "is_govcloud" {
  description = "Boolean, if this is running on GovCloud or not"
  default     = "false"
}

variable "vpc_id" {
  description = "VPC ID for the ELK stack"
}

variable "public_subnet_ids" {
  type        = "list"
  description = "Public subnet ids, where Kibana and Logstash ELBs will be placed"
}

variable "private_subnet_ids" {
  type        = "list"
  description = "Private subnet ids, where Kibana, Logstash and Elasticsearch instances will be placed. This is also the order in which nodes will be deployed in."
}

variable "user_ingress_cidrs" {
  default     = []
  description = "List of CIDRs that will have access to Kibana UI and SSH to EC2 instances"
}

variable "elk_version" {
  default = "5.6.2"
  description = "Which versions of Elasticsearch/Logstash/Kibana to install"
}

variable "elasticsearch_dns_name" {
  description = "DNS name for Elasticsearch"
}

variable "elasticsearch_dns_ssl_name" {
  default     = ""
  description = "DNS name for Elasticsearch endpoint SSL. An SSL certificate is expected to be present in ACM for this domain. If left empty 'elasticsearch_dns_name' will be checked instead."
}

variable "elasticsearch_master_node_count" {
  description = "Number of master nodes in the cluster. It should be either 1, 3 or more, in order to deal with split brain"
  default     = 1
}

variable "elasticsearch_master_node_instance_type" {
  default     = "t2.micro"
  description = "Instance type to use for master nodes."
}

variable "elasticsearch_master_node_ebs_size" {
  description = "Size of the data volume for a master node"
  default     = 4
}

variable "elasticsearch_master_node_snapshot_ids" {
  default     = [""]
  description = "List of snapshots ids to use for master nodes."
}

variable "elasticsearch_data_node_count" {
  description = "Number of data nodes in the cluster"
  default     = 1
}

variable "elasticsearch_data_node_instance_type" {
  default     = "t2.micro"
  description = "Instance type to use for data nodes."
}

variable "elasticsearch_data_node_ebs_size" {
  description = "Size of the data volume for a master node"
  default     = 16
}

variable "elasticsearch_data_node_snapshot_ids" {
  default     = [""]
  description = "List of snapshots ids to use for data nodes."
}

variable "elasticsearch_index_retention_period" {
  default     = 60
  description = "Age of Elasticsearch indices in days before they will be considered old and be pruned by the curator"
}

variable "elasticsearch_extra_setup_snippet" {
  default     = ""
  description = "Extra snippet to run after Elasticsearch has been installed and configured"
}

variable "elasticsearch_auth_elb_ingress_cidrs" {
  default     = []
  description = "CIDRs that are allowed to access Elasticsearch API over HTTPS on port 9201 with BasicAuth."
}

variable "elasticsearch_extra_config" {
  default     = ""
  description = "Extra Elasticsearch configuration in yaml format that will be applied to all nodes"
}

variable "elasticsearch_internal_alb" {
  type = "map"
  description = "Internal ALB information for Elasticsearch API"
}

variable "elasticsearch_external_alb" {
  type = "map"
  default = {}
  description = "External ALB information for Elasticsearch API secured with BasicAuth"
}

variable "elasticsearch_external_alb_setup" {
  default = false
  description = "Should external ALB be configured for Elasticsearch API. Reverse proxy with Basic Authentication will also be setup for ES API on port 9201"
}

variable "logstash_kibana_instance_type" {
  default     = "t2.micro"
  description = "Instance type to use for servers running Kibana+Logstash."
}

variable "logstash_kibana_min_server_count" {
  description = "Minimum number of EC2 instances running Logstash+Kibana"
  default     = 1
}

variable "logstash_kibana_max_server_count" {
  description = "Maximum number of EC2 instances running Logstash+Kibana"
  default     = 1
}

variable "logstash_kibana_desired_server_count" {
  description = "Desired number of EC2 instances running Logstash+Kibana"
  default     = 1
}

variable "logstash_extra_ingress_cidrs" {
  description = "Extra CIDRs that will be allowed to connect to Logstash over Beat protocol"
  default     = []
}

variable "logstash_extra_grok_patterns" {
  description = "Extra grok Patterns for Logstash, which can be used during log parsing by setting: patterns_dir => ['/etc/logstash/patterns']"
  default     = ""
}

variable "logstash_dns_name" {
  description = "DNS name for Logstash"
}

variable "kibana_alb" {
  type = "map"
  description = "ALB information for Kibana"
}

variable "certstrap_depot_path" {
  default     = ""
  description = "Local path, where generated SSL certifcates will be stored in. Certificates will be removed from local file system if left empty and will only be retained in credential store"
}

variable "certstrap_ca_force_new" {
  default     = false
  description = "New CA certificate will be created even if there already one exitsts with the same CN"
}

variable "certstrap_ca_common_name" {
  default     = "Logstash"
  description = "Common Name to be used during CA certificate generation"
}

variable "certstrap_ca_passphrase" {
  default     = ""
  description = "Passphrase for SSL Key encryption to be used during CA certificate generation"
}

variable "credstash_kms_key_arn" {
  description = "Master KMS key ARN for getting SSL server key using credstash"
}

variable "credstash_reader_policy_arn" {
  description = "Secrets Reader Policy ARN that was created by 'credstash-setup' module. Reading will be disabled if not supplied."
}

variable "credstash_install_snippet" {
  description = "Ubuntu bash script snippet for installing credstash and its dependencies"
}

variable "credstash_get_cmd" {
  description = "Credstash get command with region and table values set."
}

variable "credstash_put_cmd" {
  description = "Credstash put command with region, table and kms key values set."
}

variable "ssh_key_name" {
  default     = ""
  description = "Use an existing ssh key pair, must already be created on AWS. If empty, SSH access will be disabled."
}

variable "ami" {
  default = ""
  description = "Ubuntu AMI to use for all nodes. If left empty newest 16.04 will be used"
}
