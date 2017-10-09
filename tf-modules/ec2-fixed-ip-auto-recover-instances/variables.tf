variable "ami" {
  description = "AMI to use for instances.  Tested with Ubuntu 16.04."
}

variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}

variable "instance_type" {
  description = "Type of instance to run the servers"
  default     = "t2.nano"
}

variable "subnet_ids" {
  description = "Subnets to run servers in"
  type        = "list"
}

variable "private_ips" {
  description = "Private IP addresses of servers, which must be within the subnets specified in 'subnet_ids' (in the same order).  These are specified explicitly since it's desirable to be able to replace a DNS server without its IP address changing.  Our convention is to use the first unreserved address in the subnet (which is to say, the '+4' address)."
  type        = "list"
}

variable "security_group_ids" {
  description = "Security groups to assign servers"
  type        = "list"
}

variable "key_name" {
  description = "SSH key-pair name to use for setup"
}


variable "name_prefix" {
  description = "Name prefix of the instances (will append 'dns-master-XX')"
}

variable "name_format" {
  default     = "%s-auto-recover-%02d"
  description = "naming scheme as a string, to use with the format() function"
}

variable "user_data" {
  description = "shell script code passed to `aws_instance.user_data`"
  default     = ""
}

variable "alarm_actions" {
  description = "list of alarm actions to append to the default (optional)"
  default     = []
}

variable "root_volume_type" {
  default     = "gp2"
  description = "Type of EBS volume to use for the root block device"
}

variable "root_volume_size" {
  default     = "8"
  description = "Size (in GB) of EBS volume to use for the root block device"
}
