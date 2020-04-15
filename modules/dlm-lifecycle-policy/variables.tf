variable "name_prefix" {
  description = "Name to prefix the DML lifecycle"
  type        = string
}

variable "ebs_target_tags" {
  description = "Tags to filter the volume that we want to take the snapshot."
  type        = map
  default     = {}
}

variable "description" {
  description = "DLM lifecycle policy description"
  type        = string
  default     = "DLM lifecycle policy"
}

variable "resource_type" {
  description = "DLM resource type"
  type        = list(string)
  default     = ["VOLUME"]
}

variable "policy_name" {
  description = "Snapshots schedule name"
  type        = string
  default     = "One week of daily snapshots"
}

variable "policy_interval" {
  description = "Snapshots schedule interval"
  type        = number
  default     = 24
}

variable "policy_interval_unit" {
  description = "Snapshots schedule interval unit"
  type        = string
  default     = "HOURS"
}

variable "policy_times" {
  description = "Time at which the snapshot will take."
  type        = list
  default     = ["23:45"]
}

variable "policy_retain_rule" {
  description = "Snapshots schedule retein rule, how many snapshots are retaining"
  type        = number
  default     = 14
}

variable "policy_copy_tags" {
  description = "Copy all user-defined tags on a source volume to snapshots of the volume created by this policy."
  type        = bool
  default     = true
}

variable "policy_tags_to_add" {
  description = "Tags to add to the snapshot"
  type        = map
  default     = {"SnapshotCreator" = "DLM lifecycle"}
}

variable "iam_role_name" {
  description = "The IAM role name for the DLM lifecyle policy"
  type        = string
  default     = "dlm-lifecycle-role"
}
