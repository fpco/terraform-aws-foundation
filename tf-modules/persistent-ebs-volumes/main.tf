/**
 * ## Create an arbitrary number of EBS volumes
 *
 * This module provides EBS volumes and associated IAM policies to be
 * used with an EC2 instances or auto-scaling groups. This module is best when
 * used in conjunction with a single-node auto-scaling group, and
 * `volume-mount-snippets` can be used to attache EBS volumes on boot.
 * Volumes created will be interleaved throughout the Avaialability Zones
 *
 */
resource "aws_ebs_volume" "volumes" {
  count             = "${var.volume_count}"
  availability_zone = "${element(var.azs, count.index)}"
  size              = "${var.size}"
  type              = "${var.volume_type}"

  encrypted   = "${var.encrypted}"
  kms_key_id  = "${var.encrypted ? var.kms_key_id : ""}"
  snapshot_id = "${element(var.snapshot_ids, count.index)}"
  tags        = "${merge(map("Name", "${var.name_prefix}-${format("%02d", count.index + 1)}-${element(var.azs, count.index)}"), "${var.extra_tags}")}"
}

data "template_file" "volume_mount_snippets" {
  count    = "${var.volume_count}"
  template = "${file("${path.module}/snippet.tpl.sh")}"
  vars {
    volume_id     = "${element(aws_ebs_volume.volumes.*.id, count.index)}"
    device_name   = "${var.device_name}"
    wait_interval = "${var.wait_interval}"
    max_wait      = "${var.max_wait}"
  }
}
