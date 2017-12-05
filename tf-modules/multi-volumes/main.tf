/**
 * ## Multi-Volumes (EBS)
 *
 * Easily create multiple volumes from a comma-separated list of sizes.
 *
 * This is currently limited to five volumes, but can be expanded by
 * adding more aws_ebs_volumes and aws_volume_attachments following
 * the established pattern.
 *
 * NOTE: this module could be updated to support N number of volumes.
 *
 * **TODO: REVIEW THE OTHER MODULES AND ASK THE ENGINEERS, IS THIS MODULE STILL IN USE?**
 *
 */

variable "instance_id" {
    description = "Instance to attach volumes to"
}
variable "availability_zone" {
    description = "Availability zone to create volumes in"
}
variable "sizes" {
    description = "Comma-separated list of volume sizes"
}
variable "name" {
    description = "Prefix for names of volumes (will have '_n' where n is the volume number appended)"
}

# This is a used with lookup and signum in order to to conditionally create data
# volumes, providing the same logic as "if length(sizes) > N".
# Unfortunately, we can't just use 'count' to iterate through sizes
# because the way TF handles dependencies/changes currently means all
# attachments are re-created if a new one is added
variable "sigpos" {
  default = {
    "-1" = "0"
    "0" = "0"
    "1" = "1"
  }
}

resource "aws_ebs_volume" "volume0" {
    count = "${lookup(var.sigpos, signum(length(split(",", var.sizes))))}"
    availability_zone = "${var.availability_zone}"
    size = "${element(split(",", var.sizes), 0)}"
    type = "gp2"
    encrypted = true
    tags {
        Name = "${var.name}_0"
    }
}
resource "aws_volume_attachment" "volume0" {
    count = "${lookup(var.sigpos, signum(length(split(",", var.sizes))))}"
    device_name = "/dev/sdf"
    volume_id = "${aws_ebs_volume.volume0.id}"
    instance_id = "${var.instance_id}"
}

resource "aws_ebs_volume" "volume1" {
    count = "${lookup(var.sigpos, signum(length(split(",", var.sizes)) - 1))}"
    availability_zone = "${var.availability_zone}"
    size = "${element(split(",", var.sizes), 1)}"
    type = "gp2"
    encrypted = true
    tags {
        Name = "${var.name}_1"
    }
}
resource "aws_volume_attachment" "volume1" {
    count = "${lookup(var.sigpos, signum(length(split(",", var.sizes)) - 1))}"
    device_name = "/dev/sdg"
    volume_id = "${aws_ebs_volume.volume1.id}"
    instance_id = "${var.instance_id}"
}

resource "aws_ebs_volume" "volume2" {
    count = "${lookup(var.sigpos, signum(length(split(",", var.sizes)) - 2))}"
    availability_zone = "${var.availability_zone}"
    size = "${element(split(",", var.sizes), 2)}"
    type = "gp2"
    encrypted = true
    tags {
        Name = "${var.name}_2"
    }
}
resource "aws_volume_attachment" "volume2" {
    count = "${lookup(var.sigpos, signum(length(split(",", var.sizes)) - 2))}"
    device_name = "/dev/sdh"
    volume_id = "${aws_ebs_volume.volume2.id}"
    instance_id = "${var.instance_id}"
}

resource "aws_ebs_volume" "volume3" {
    count = "${lookup(var.sigpos, signum(length(split(",", var.sizes)) - 3))}"
    availability_zone = "${var.availability_zone}"
    size = "${element(split(",", var.sizes), 3)}"
    type = "gp2"
    encrypted = true
    tags {
        Name = "${var.name}_3"
    }
}
resource "aws_volume_attachment" "volume3" {
    count = "${lookup(var.sigpos, signum(length(split(",", var.sizes)) - 3))}"
    device_name = "/dev/sdi"
    volume_id = "${aws_ebs_volume.volume3.id}"
    instance_id = "${var.instance_id}"
}

resource "aws_ebs_volume" "volume4" {
    count = "${lookup(var.sigpos, signum(length(split(",", var.sizes)) - 4))}"
    availability_zone = "${var.availability_zone}"
    size = "${element(split(",", var.sizes), 4)}"
    type = "gp2"
    encrypted = true
    tags {
        Name = "${var.name}_4"
    }
}
resource "aws_volume_attachment" "volume4" {
    count = "${lookup(var.sigpos, signum(length(split(",", var.sizes)) - 4))}"
    device_name = "/dev/sdj"
    volume_id = "${aws_ebs_volume.volume4.id}"
    instance_id = "${var.instance_id}"
}
