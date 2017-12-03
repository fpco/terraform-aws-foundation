variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}

variable "ami" {
  description = "AMI to use for instances.  Tested with Ubuntu 16.04."
  type        = "string"
}

variable "instance_type" {
  description = "Type of instance to run the DNS servers"
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
  type        = "string"
}

variable "ssh_key" {
  description = "File path to the private key for SSH"
  default     = "/dev/null"
}

variable "name" {
  description = "Name prefix of the instances (will have server number appended).  One of 'name' or 'names' may be specified."
  default     = "dns"
}

variable "names" {
  description = "List of names for each instance.  One of 'name' or 'names' may be specified."
  default     = []
}

variable "named_conf" {
  description = "Complete content of '/etc/bind/named.conf'."
  default     = "//"
}

variable "named_conf_options" {
  description = "Complete content of '/etc/bind/named.conf.options'."
  default     = "//"
}

variable "named_conf_local" {
  description = "Complete content of '/etc/bind/named.conf.local'."
  default     = "//"
}

variable "db_records_folder" {
  description = "Path to locally rendered directory of zone files."
  default     = ""
}

variable "log_files" {
  description = "A list of configured log files.  These will be created with correct ownership before reloading configuration."
  default     = []
}

variable "distro" {
  description = "Linux distribution that AMI runs: 'ubuntu' or 'amazon'"
  default     = "ubuntu"
}

variable "alarm_actions" {
  description = "list of alarm actions to append to the default (optional)"
  default     = []
}

variable "bastion_host" {
  default     = ""
  description = "maps to `bastion_host` attribute for the `ssh` `connection` to the `aws_instance` provisioners"
}

variable "bastion_user" {
  default     = ""
  description = "maps to `bastion_user` attribute for the `ssh` `connection` to the `aws_instance` provisioners"
}

variable "bastion_private_key" {
  default     = ""
  description = "maps to `bastion_private_key` attribute for the `ssh` `connection` to the `aws_instance` provisioners"
}
