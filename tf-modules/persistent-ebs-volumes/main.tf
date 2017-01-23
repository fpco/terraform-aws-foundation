/**
 *## Creation of an arbitrary number of EBS volumes
 *
 * Volumes created will be interleaved throughout the Avaialability Zones
 *
 */
resource "aws_ebs_volume" "volumes" {
  count = "${var.volume_count}"
  availability_zone = "${element(var.azs, count.index)}"
  size = "${var.size}"
  type = "${var.volume_type}"
  
  # encrypted   = "${var.encrypted}"
  # kms_key_id  = "${var.kms_key_id}"
  snapshot_id = "${element(var.snapshot_ids, count.index)}"
  tags {
    Name = "${var.name_prefix}-${count.index}-${element(var.azs, count.index)}"
  }
}
