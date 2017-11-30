variable "is_govcloud" {
  description = "If this is running on GovCloud or not"
  default     = false
}

variable "instance_type" {
  description = "AWS instance type, use larger instances for high-volume traffic"
  default     = "t2.nano"
}

variable "name_prefix" {
  description = "Prefix for naming resources, usually project-related"
  type        = "string"
}

variable "extra_tags" {
  description = "map of tags to append to the Name tag, added to the instance"
  default     = {}
}

variable "public_subnet_id" {
  description = "the ID of the subnet to deploy the NAT instance to"
  type        = "string"
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
