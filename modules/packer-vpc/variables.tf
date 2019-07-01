#
variable "name" {
  default     = "packer"
  description = "The name of the image build environment"
  type        = string
}

#
variable "region" {
  default     = "us-west-1"
  description = "The AWS region to deploy to"
  type        = string
}

#
variable "vpc_cidr_prefix" {
  default     = "10.72"
  description = "The IP prefix to the CIDR block assigned to the VPC"
  type        = string
}

