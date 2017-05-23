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
}

variable "vpc_azs" {
  type = "list"
  description = "A list of availability zones to deploy subnets in"
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

variable "extra_elb_ingress_cidrs" {
  default = []
  description = "Extra CIDRs that are allowed to access Elasticsearch API. By default only CIDR from `public_subnet_ids` are allowed"
}

variable "internal" {
  default = true
  description = "Set it to false if you want Elasticsearch to be accessible by the outside world"
}

variable "route53_zone_id" {
  description = "Route53 Zone id where ELB should get added a record to"
}

variable "elasticsearch_dns_name" {
  description = "DNS name for Elasticsearch"
  default = "elasticsearch-dev.e1c.net"
}

variable "key_name" {
  description = "SSH key name to use for connecting to all nodes"
}

