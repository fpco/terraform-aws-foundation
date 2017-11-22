variable "name_prefix" {
  description = "The name prefix of the autoscaling group"
}

variable "key_name" {
  description = "The name of the keypair to use"
}

variable "instance_type" {
  default     = "t2.medium"
  description = "The type of the EC2 instance to use (needs to be at least t2.medium)"
}

variable "instance_ami" {
  default     = "ami-cd0f5cb6"
  description = "The EC2 image to use"
}

variable "security_group_ids" {
  type        = "list"
  description = "The security groups to use for instances"
}

variable "region" {
  description = "The region for all of the resources"
}

variable "az" {
  description = "The availability zone to create the instance in"
}

variable "vpc_id" {
  description = "The VPC to put the security group on"
}

variable "subnet_id" {
  description = "The VPC subnet to put the instance on"
}

variable "root_volume_type" {
  default     = "gp2"
  description = "Type of EBS volume to use for the root block device"
}

variable "root_volume_size" {
  default     = "8"
  description = "Size (in GB) of EBS volume to use for the root block device"
}

variable "data_volume_type" {
  default     = "gp2"
  description = "Type of EBS volume to use for the EBS volume"
}

variable "data_volume_size" {
  default     = "10"
  description = "Size (in GB) of EBS volume to use for the EBS volume"
}

variable "data_volume_encrypted" {
  default     = "true"
  description = "Boolean, whether or not to encrypt the EBS block device"
}

variable "data_volume_kms_key_id" {
  default     = ""
  description = "ID of the KMS key to use when encyprting the EBS block device"
}

variable "data_volume_snapshot_id" {
  default     = ""
  description = "The ID of the snapshot to base the EBS block device on"
}

variable "data_volume_iops" {
  default     = ""
  description = "The amount of IOPS to provision for the EBS block device"
}

variable "gitlab_ssh_port" {
  default     = "8022"
  description = "The port to use for ssh access to the gitlab instance"
}

variable "gitlab_http_port" {
  default     = "80"
  description = "The port to use for http access to the gitlab instance"
}

variable "gitlab_https_port" {
  default     = "443"
  description = "The port to use for https access to the gitlab instance"
}

variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}
