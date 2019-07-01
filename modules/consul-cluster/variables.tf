variable "name" {
  description = "The name of this auto-scaling cluster, this should be unique"
  type = string
}

variable "key_name" {
  description = "The name of the (AWS) SSH key to associate with the instance"
  type = string
}

variable "ami" {
  description = "The base AMI for each AWS instance created"
  type = string
}

variable "iam_profile" {
  default     = ""
  description = "The IAM profile to associate with AWS instances in the ASG"
  type = string
}

variable "instance_type" {
  default     = "t2.micro"
  description = "The type of AWS instance (size)"
  type = string
}

variable "user_data" {
  default     = ""
  description = "The user_data string to pass to cloud-init"
  type = string
}

variable "cluster_security_group_ids" {
  default     = []
  description = "List of IDs for security groups to associate with the cluster"
  type = list(string)
}

variable "load_balancers" {
  default     = []
  description = "The list of names of load balancers to pass to the ASG module"
  type = list(string)
}

variable "max_nodes" {
  default     = 9
  description = "The maximum number of nodes in each group"
  type = number
}

variable "min_nodes" {
  default     = 3
  description = "The minimum number of nodes in each group"
  type = number
}

variable "cidr_minions_a" {
  default     = "10.100.7.0/24"
  description = "The CIDR block for subnet a"
  type = string
}

variable "cidr_minions_c" {
  default     = "10.100.8.0/24"
  description = "The CIDR block for subnet c"
  type = string
}

variable "region" {
  description = "AWS region to deploy to"
  default     = ""
  type = string
}

variable "vpc_id" {
  description = "The ID of the experimental VPC to deploy to, depends on the AWS region"
  type = string
}

variable "route_table_id" {
  description = "The ID of the experimental routing table to use, depends on the AWS region"
  type = string
}

variable "public_ip" {
  default     = true
  description = "Boolean flag to enable/disable `map_public_ip_on_launch` in each `aws_subnet`"
  type = bool
}

variable "root_volume_type" {
  default     = "gp2"
  description = "Type of EBS volume to use for the root block device"
  type = string
}

variable "root_volume_size" {
  default     = 15
  description = "Size (in GB) of EBS volume to use for the root block device"
  type = number
}
