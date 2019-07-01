variable "name_prefix" {
  description = "The name prefix EBS for volumes"
  type        = string
}

variable "volume_count" {
  description = "How many EBS volumes should be deployed accross all AZs"
  type        = number
}

variable "azs" {
  type        = list(string)
  description = "AWS Availability Zones (AZs) to create the volumes in"
}

variable "volume_type" {
  default     = "gp2"
  description = "Type of EBS volume to use for the EBS block device"
  type        = string
}

variable "size" {
  default     = "15"
  description = "Size (in GB) of EBS volume to use for the EBS block device"
  type        = number
}

variable "snapshot_ids" {
  type        = list(string)
  description = "IDs of the snapshots to base the EBS block devices on"
}

variable "encrypted" {
  default     = false
  description = "Boolean, whether or not to encrypt EBS block devices"
  type        = bool
}

variable "kms_key_id" {
  default     = ""
  description = "ID of the KMS key to use when encrypting EBS block devices"
  type        = string
}

variable "device_name" {
  default     = "/dev/xvdf"
  description = "Setting for volume mount snipppet. Name of device volume should be mounted as"
  type        = string
}

variable "wait_interval" {
  default     = 1
  description = "Setting for volume mount snipppet. Number of seconds to wait between retries"
  type        = number
}

variable "max_wait" {
  default     = 60
  description = "Setting for volume mount snipppet. Maximum number of seconds to wait for volume to be mounted before exiting with an error"
  type        = number
}

variable "extra_tags" {
  default     = {}
  description = "Map with extra tags to be applied to all ebs volumes"
  type        = map(string)
}

