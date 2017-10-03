variable "is_govcloud" {
  description = "Boolean, is it running on GovCloud"
  default     = false
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
  description = "Master nodes EC2 instance type"
  default = "t2.micro"
}

variable "data_node_instance_type" {
  description = "Data nodes EC2 instance type"
  default = "t2.micro"
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
  description = "VPC id where Elasticsearch cluster is deployed"
}

variable "private_subnet_ids" {
  type = "list"
  description = "Private subnet ids where instances will be deployed in."
}

variable "public_subnet_ids" {
  type = "list"
  description = "Public subnet ids where ELB will be deployed in. Pass private subnet ids, if you expect elasticsearch to be a private resource only."
}

variable "extra_sg_ids" {
  default = []
  description = "Extra Security Group IDs that will be added to all Elasticvsearch nodes. This is a way to add extra services, such as SSH access for instance."
}

variable "extra_elb_sg_ids" {
  default = []
  description = "Extra Security Group IDs that will be added to Elasticsearch API Load Balancer"
}

variable "extra_setup_snippet" {
  default = ""
  description = "Extra snippet to run after Elasticsearch has been installed and configured"
}

variable "extra_config" {
  default = ""
  description = "Extra elasticsearch configuration in yaml format that will be applied to all nodes"
}

variable "elasticsearch_dns_name" {
  description = "DNS name for Elasticsearch"
}

variable "key_name" {
  description = "SSH key name to use for connecting to all nodes"
}

variable "deploy_curator" {
  default = false
  description = "Should a curator be installed and enabled on all master eligible nodes. If enabled it will only run on currently elected master."
}

variable "index_retention_period" {
  default = 60
  description = "Age of Elasticsearch indices in days before they will be considered old and be pruned by the curator. Set to 0 in order to disable."
}

variable "extra_curator_actions" {
  default = ""
  description = "YAML formatted dictionary of actions, as described in documentation, but started at index '2', since action number '1' is the one that purges old indices."
}

variable "logstash_beats_address" {
  description = "DNS name and port of where logstash is listenting with beats protocol."
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


variable "internal_alb" {
  type = "map"
  description = "Information about Internal ALB"
}

variable "external_alb_setup" {
  default = false
  description = "Should an external access be allowed to ES API on port 9201 with HTTPS and BasicAuth."
}

variable "elasticsearch_dns_ssl_name" {
  default = ""
  description = "DNS name for Elasticsearch endpoint SSL. An SSL certificate is expected to be present in ACM for this domain. If left empty 'elasticsearch_dns_name' will be checked instead."
}

variable "external_alb" {
  type = "map"
  default = {}
  description = "Information about External ALB"
}

variable "external_alb_ingress_cidrs" {
  default = []
  description = "CIDRs that are allowed to access Elasticsearch API over HTTPS on port 9201 with BasicAuth."
}

variable "elasticsearch_version" {
  default = "5.6.2"
  description = "Which version of elasticsearch to install"
}
