/**
 *## Persistent Data on EBS
 *
 * This module provides an EBS volume and associated IAM profile/role to be
 * used with an EC2 instance or auto-scaling group. This module is best when
 * used in conjunction with a single-node auto-scaling group, and with the
 * init-snippet that attaches the named EBS volume on boot.
 */
resource "aws_ebs_volume" "main" {
    availability_zone = "${var.az}"
    size = "${var.size}"
    type = "${var.volume_type}"
#   iops = "${var.iops}"
    encrypted   = "${var.encrypted}"
    kms_key_id  = "${var.kms_key_id}"
    snapshot_id = "${var.snapshot_id}"
    tags        = "${merge(map("Name", "${var.name}"), "${var.extra_tags}")}"
}
//`id` exported from the `aws_iam_instance_profile`
output "iam_profile" {
    value = "${aws_iam_instance_profile.attach_ebs.id}"
}
//`id` exported from the `aws_ebs_volume`
output "volume_id" {
    value = "${aws_ebs_volume.main.id}"
}
