variable "name_prefix" {
  description = "name for the env, prefixes the suffix"
  type        = string
}

variable "name_suffix" {
  description = "portion of the name to append to the `name_prefix`"
  type        = string
  default     = "workspace"
}

variable "region" {
  default = "us-east-2"
  type    = string
}

variable "instance_type" {
  description = "the type of EC2 instance"
  default     = "t2.small"
  type        = string
}

variable "vpc_cidr" {
  description = "Network CIDR for the VPC"
  type        = string
  default     = "192.168.0.0/24"
}

variable "ssh_pubkey" {
  description = "The path to the SSH pub key to use"
  default     = "./id_rsa.pub"
}

variable "dns_zone_name" {
  description = "The name of the DNS zone on Route53, to create records in for the workspace"
  type        = string
}

variable "init_suffix" {
  description = "shell code to run at the end of init, after default provisioning"
  type        = string
  default     = ""
}

variable "extra_tags" {
  description = "tags to append/merge with existing tags on select resources"
  type        = map(string)
  default     = {}
}

