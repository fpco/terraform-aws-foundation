/**
 * ## Persistent Data on EBS
 *
 * This module provides an EBS volume and associated IAM profile/role to be
 * used with an EC2 instance or auto-scaling group. This module is best when
 * used in conjunction with a single-node auto-scaling group, and with the
 * init-snippet that attaches the named EBS volume on boot.
 *
 */

resource "aws_ebs_volume" "main" {
  availability_zone = "${var.az}"
  size              = "${var.size}"
  type              = "${var.volume_type}"
  encrypted         = "${var.encrypted}"
  kms_key_id        = "${var.kms_key_id}"
  snapshot_id       = "${var.snapshot_id}"
  # merge Name w/ extra_tags
  tags = "${merge(map("Name", "${var.name_prefix}-${var.az}"), "${var.extra_tags}")}"
}

// `id` exported from the `aws_iam_instance_profile`
output "iam_profile_id" {
  value = "${aws_iam_instance_profile.attach_ebs.id}"
}

// `arn` exported from the `aws_iam_instance_profile`
output "iam_profile_arn" {
  value = "${aws_iam_instance_profile.attach_ebs.arn}"
}

// `policy` exported from the `aws_iam_role_policy`
output "iam_profile_policy_document" {
  value = "${aws_iam_role_policy.attach_ebs.policy}"
}

// `arn` exported from the `aws_iam_role`
output "iam_role_arn" {
  value = "${aws_iam_role.attach_ebs.arn}"
}

// `name` exported from the `aws_iam_role`
output "iam_role_name" {
  value = "${aws_iam_role.attach_ebs.name}"
}

// `id` exported from the `aws_ebs_volume`
output "volume_id" {
  value = "${aws_ebs_volume.main.id}"
}
