variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
  type        = "string"
}

variable "instance_type" {
  description = "AWS instance type, use larger instances for high-volume traffic"
  default     = "t2.nano"
}

variable "root_volume_size" {
  description = "size (in GB) of the EBS root volume for the EC2 instances"
  default     = "10"
  type        = "string"
}

variable "name_prefix" {
  description = "Prefix for naming resources, usually project-related"
  type        = "string"
}

variable "extra_tags" {
  description = "map of tags to append to the Name tag, added to the instance"
  default     = {}
}

variable "public_subnet_ids" {
  description = "list of IDs for subnets to deploy the NAT instances into"
  type        = "list"
}

variable "private_subnet_cidrs" {
  description = "CIDRs of private subnets for iptables configuration"
  type        = "list"
}

variable "key_name" {
  description = "name of SSH key, maps to NAT instance `key_name`"
  type        = "string"
}

variable "security_group_ids" {
  description = "list of security groups to associate with the NAT instance"
  type        = "list"
}

variable "iam_profiles" {
  description = "list of iam profiles to associate with each EC2 instance"
  type        = "list"
  default     = []
}
