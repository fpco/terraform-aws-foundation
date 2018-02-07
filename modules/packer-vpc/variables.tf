#
variable "name" {
  default     = "packer"
  description = "The name of the image build environment"
}

#
variable "region" {
  default     = "us-west-1"
  description = "The AWS region to deploy to"
}

#
variable "vpc_cidr_prefix" {
  default     = "10.72"
  description = "The IP prefix to the CIDR block assigned to the VPC"
}
