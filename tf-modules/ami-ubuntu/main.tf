/**
 * ## Ubuntu AMI lookup helper
 *
 * This module is a simple helper that looks up the current AMI ID for a given
 * release of ubuntu. Use it like:
 *
 * ```
 * module "ubuntu-xenial-ami" {
 *   source  = "../../tf-modules/ami-ubuntu"
 *   release = "14.04"
 * } 
 * ```
 *
 * Then reference with: `${module.ubuntu-xenial-ami.id}`
 *
 * If you deploy an instance with this AMI, and later do a `terraform plan`, the
 * most recent AMI will be looked up, and that may change the AMI for the instance.
 * You can use `ignore_changes` or `-target`, depending on the type of workflow
 * you would like to apply.
 *
 */

variable "most_recent" {
  default     = true
  description = "boolean, maps to `most_recent` parameter for `aws_ami` data source"
}

variable "name_map" {
  default = {
    "16.04" = "xenial"
    "14.04" = "trusty"
  }

  description = "map of release numbers to names"
}

variable "release" {
  default     = "16.04"
  description = "default ubuntu release to target"
}

variable "is_govcloud" {
  default     = false
  description = "boolean, defines `owners` below, as govcloud carries different ids"
}

data "aws_ami" "ubuntu" {
  most_recent = "${var.most_recent}"

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-${var.name_map[var.release]}-${var.release}-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["${var.is_govcloud == "true" ? "513442679011" : "099720109477"}"] # Canonical
}

// ID of the AMI
output "id" {
  value = "${data.aws_ami.ubuntu.id}"
}
