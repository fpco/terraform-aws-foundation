variable "name_prefix" {
  description = "The name of the environment to deploy to (beta/prod/etc)"
  type        = string
}

variable "region" {
  description = "The AWS region to deploy to"
  default     = ""
  type        = string
}

variable "az" {
  description = "The AWS Availability Zone (AZ) to create the instance in"
  type        = string
}

variable "volume_type" {
  default     = "gp2"
  description = "Type of EBS volume to use for the EBS block device"
  type        = string
}

variable "size" {
  default     = "15"
  description = "Size (in GB) of EBS volume to use for the EBS block device"
  type        = string
}

variable "snapshot_id" {
  default     = ""
  description = "The ID of the snapshot to base the EBS block device on"
  type        = string
}

variable "encrypted" {
  default     = true
  description = "Boolean, whether or not to encrypt the EBS block device"
  type        = string
}

variable "kms_key_id" {
  default     = ""
  description = "ID of the KMS key to use when encyprting the EBS block device"
  type        = string
}

variable "iops" {
  default     = ""
  description = "The amount of IOPS to provision for the EBS block device"
  type        = string
}

variable "extra_tags" {
  default     = {}
  description = "Map with extra tags to be applied to all ebs volumes"
  type        = map(string)
}

