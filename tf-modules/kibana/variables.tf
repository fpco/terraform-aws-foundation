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

variable "vpc_azs" {
  description = "A list of availability zones to deploy Kibana servers in"
  type = "list"
}

variable "private_subnet_ids" {
  description = "A list of private subnet ids to deploy Kibana servers in"
  type = "list"
}

variable "public_subnet_ids" {
  description = "A list of public subnet ids to deploy Kibana ELB in"
  type = "list"
}

variable "kibana_dns_name" {
  description = "DNS name for Kibana endpoint (An SSL certificate is expected in ACM for this domain)"
}

variable "route53_zone_id" {
  description = "Route53 Zone id where ELB should get added a record to"
}

variable "elasticsearch_url" {
  description = "Elasticsearch endpoint URL"
}

variable "key_name" {
  description = "SSH key name to use for connecting to all nodes"
}

variable "internal" {
  default = true
  description = "Set it to false if you want Kibana to be accessible by the outside world"
}
