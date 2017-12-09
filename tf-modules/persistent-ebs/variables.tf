variable "name_prefix" {
  description = "The name of the environment to deploy to (beta/prod/etc)"
}

variable "region" {
  description = "The AWS region to deploy to"
  default     = ""
}

variable "az" {
  description = "The AWS Availability Zone (AZ) to create the instance in"
}

variable "volume_type" {
  default     = "gp2"
  description = "Type of EBS volume to use for the EBS block device"
}

variable "size" {
  default     = "15"
  description = "Size (in GB) of EBS volume to use for the EBS block device"
}

variable "snapshot_id" {
  default     = ""
  description = "The ID of the snapshot to base the EBS block device on"
}

variable "encrypted" {
  default     = "true"
  description = "Boolean, whether or not to encrypt the EBS block device"
}

variable "kms_key_id" {
  default     = ""
  description = "ID of the KMS key to use when encyprting the EBS block device"
}

variable "iops" {
  default     = ""
  description = "The amount of IOPS to provision for the EBS block device"
}

variable "extra_tags" {
  default     = {}
  description = "Map with extra tags to be applied to all ebs volumes"
}

variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}
