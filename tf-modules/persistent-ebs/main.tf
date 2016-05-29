provider "aws" {
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region     = "${var.region}"
}
resource "aws_ebs_volume" "main" {
    availability_zone = "${var.az}"
    size = "${var.size}"
    type = "${var.volume_type}"
#   iops = "${var.iops}"
    encrypted   = "${var.encrypted}"
    kms_key_id  = "${var.kms_key_id}"
    snapshot_id = "${var.snapshot_id}"
    tags {
        Name = "${var.name}"
    }
}
output "iam_profile" {
    value = "${aws_iam_instance_profile.attach_ebs.id}"
}
output "volume_id" {
    value = "${aws_ebs_volume.main.id}"
}
