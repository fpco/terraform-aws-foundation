/**
* ## Data Lifecycle Manager (DLM) lifecycle policy for managing snapshots
*
* The purpose of this module is to provide a policy to create snapshots accourding an schudule.
* This module creates an IAM role and a policy that manage the creation of EBS snapshots, Data Lifecycle Manager policy let you create snapshots according to the schedule that you choose.
*
* The module supports:
*
* * Generate snapshots of volumes attached to ec2 instances
* * Assume the IAM Role policy to manage DLM lifecycle policy
*
*/

# DLM lifecycle schedule
resource "aws_dlm_lifecycle_policy" "ebs-lifecycle-policy" {
  description        = var.description
  execution_role_arn = aws_iam_role.dlm_lifecycle_role.arn
  state              = "ENABLED"

  policy_details {
    resource_types = var.resource_type

    schedule {
      name = "${var.name_prefix} ${var.policy_name}"

      create_rule {
        interval      = var.policy_interval
        interval_unit = var.policy_interval_unit
        times         = var.policy_times
      }

      retain_rule {
        count = var.policy_retain_rule
      }

      tags_to_add = var.policy_tags_to_add
      copy_tags   = var.policy_copy_tags
    }

    target_tags = var.ebs_target_tags

  }
}
