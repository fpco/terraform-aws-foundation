## Data Lifecycle Manager (DLM) lifecycle policy for managing snapshots

This module creates an IAM role and a policy that manage the creation of EBS snapshots, Data Lifecycle Manager policy let you create snapshots according to the schedule that you choose.

### Example how to use

Define the module in your terraform project:
```
variable "ebs_target_tags" {
  description = "EBS name/tag to query"
  default     = "myebstagname"
}

Define variables
...

module "ebs-backup-policy" {
  source = "github.com/fpco/terraform-aws-foundation//modules/dlm-lifecycle-policy"

  name_prefix               = "${var.name}"
  dml_description           = "${var.dml_description}"
  ebs_target_tags           = "${merge(map("Name", "${var.ebs_target_tags}"), "${var.extra_tags}")}"
  schedule_create_interval  = "${var.schedule_create_interval}"
  schedule_create_time      = "${var.schedule_create_time}"
  schedule_retain_rule      = "${var.schedule_retain_rule}"
  schedule_tags_to_add      = "${merge(map("Name", "${var.name}-dlm", "SnapshotCreator", "DLM lifecycle"))}"
}
```
