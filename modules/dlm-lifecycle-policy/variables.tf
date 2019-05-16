variable "name_prefix" {
  description = "Name to prefix the DML lifecycle"
  type        = "string"
}

variable "create_dlm_iam_role" {
  description = "Creates DLM IAm role and permissions to create snapshots, the role can be create one time pero AWS account"
  default     = "false"
}

variable "ebs_target_tags" {
  description = "Tags to filter the volume that we want to take the snapshot."
  default     = {}
}

variable "dml_description" {
  description = "DLM lifecycle policy description"
  default     = "DLM lifecycle policy"
}

variable "dml_resource_type" {
  description = "DLM resource type"
  default     = ["VOLUME"]
}

variable "schedule_name" {
  description = "Snapshots schedule name"
  default     = "One week of daily snapshots"
}


variable "schedule_create_interval" {
  description = "Snapshots schedule interval"
  default     = 24
}

variable "schedule_create_interval_unit" {
  description = "Snapshots schedule interval unit"
  default     = "HOURS"
}

variable "schedule_create_time" {
  description = "Time at which the snapshot will take."
  type        = "list"
  default     = ["23:45"]
}

variable "schedule_retain_rule" {
  description = "Snapshots schedule retein rule, how many snapshots are retaining"
  default     = 14
}

variable "schedule_copy_tags" {
  description = "Copy all user-defined tags on a source volume to snapshots of the volume created by this policy."
  default     = false
}

variable "schedule_tags_to_add" {
  description = "Tags to add to the snapshot"
  type        = "map"
  default     = {"SnapshotCreator" = "DLM lifecycle"}
}

variable "role_name" {
  description = "The IAM role name for the DLM lifecyle policy"
  default     = "dlm-lifecycle-role"
}
