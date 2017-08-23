variable "name_prefix" {
  description = "The name prefix EBS for volumes"
}
variable "volume_count" {
  description = "How many EBS volumes should be deployed accross all AZs"
}
variable "azs" {
  type = "list"
  description = "AWS Availability Zones (AZs) to create the volumes in"
}
variable "volume_type" {
  default = "gp2"
  description = "Type of EBS volume to use for the EBS block device"
}
variable "size" {
  default = "15"
  description = "Size (in GB) of EBS volume to use for the EBS block device"
}
variable "snapshot_ids" {
  type = "list"
  description = "IDs of the snapshots to base the EBS block devices on"
}
variable "encrypted" {
  default = "false"
  description = "Boolean, whether or not to encrypt EBS block devices"
}
variable "kms_key_id" {
  default = ""
  description = "ID of the KMS key to use when encrypting EBS block devices"
}
variable "device_name" {
  default = "/dev/xvdf"
  description = "Setting for volume mount snipppet. Name of device volume should be mounted as"
}
variable "wait_interval" {
  default = 1
  description = "Setting for volume mount snipppet. Number of seconds to wait between retries"
}
variable "max_wait" {
  default = 60
  description = "Setting for volume mount snipppet. Maximum number of seconds to wait for volume to be mounted before exiting with an error"
}
variable "extra_tags" {
  default = {}
  description = "Map with extra tags to be applied to all ebs volumes"
}
variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}
