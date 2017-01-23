variable "name_prefix" {
  description = "The name prefix EBS for volumes"
}
variable "volume_count" {
  description = "How many EBS volumes should be deployed accross all AZs"
}
variable "azs" {
  type = "list"
  description = "The AWS Availability Zones (AZs) to create the volumes in"
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
  description = "The ID of the snapshot to base the EBS block device on"
}
## TODO: had to disable optional encryption. Terraform 8 does extra regex
## validation on kms_key_id, i.e. empty string are no longer accepted.
# variable "encrypted" {
#   default = "true"
#   description = "Boolean, whether or not to encrypt the EBS block device"
# }
# variable "kms_key_id" {
#   default = ""
#   description = "ID of the KMS key to use when encyprting the EBS block device"
# }
