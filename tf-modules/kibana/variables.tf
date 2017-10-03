variable "name_prefix" {
}

variable "ami" {
}

variable "instance_type" {
  default = "t2.micro"
}

variable "min_server_count" {
  description = "Minimum number of EC2 instances running Kibana"
  default = 1
}

variable "max_server_count" {
  description = "Maximum number of EC2 instances running Kibana"
  default = 1
}

variable "desired_server_count" {
  description = "Desired number of EC2 instances running Kibana"
  default = 1
}

variable "vpc_id" {
  description = "VPC id where Kibana servers should be deployed in"
}

variable "private_subnet_ids" {
  description = "A list of private subnet ids to deploy EC2 instances running Kibana in"
  type = "list"
}

variable "elasticsearch_url" {
  description = "Elasticsearch endpoint URL"
}

variable "key_name" {
  description = "SSH key name to use for connecting to all nodes"
}

variable "extra_sg_ids" {
  default = []
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
  description = "Information about ALB"
}

variable "kibana_version" {
  default = "5.6.2"
  description = "Which version of Kibana to install"
}
