variable "name_prefix" {
  description = "Prefix for naming resources, usually project-related"
  type        = string
}

variable "name_suffix" {
  description = "suffix to include when naming the various resources"
  type        = string
  default     = ""
}

variable "region" {
  description = "The AWS region to deploy to"
  type        = string
}

variable "ami" {
  description = "The base AMI for each AWS instance created"
  type        = string
}

variable "instance_type" {
  description = "The type of AWS instance (size)"
  type        = string
}

variable "vpc_id" {
  description = "ID of VPC to associate SG with"
  type        = string
}

variable "subnet_id" {
  description = "The ID of the subnet to use, depends on the availability zone"
  type        = string
}

variable "extra_tags" {
  description = "map of name,value pairs to tag the security group (append to Name tag)"
  default     = {}
  type        = map(string)
}
