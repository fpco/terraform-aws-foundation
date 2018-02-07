/**
 * ## Ubuntu AMI lookup helper
 *
 * This module is a simple helper that looks up the current AMI ID for a given
 * release of ubuntu. Use it like:
 *
 * ```
 * module "ubuntu-xenial-ami" {
 *   source  = "../../tf-modules/ami-ubuntu"
 * }
 * ```
 *
 * Or for Trusty:
 *
 * ```
 * module "ubuntu-trusty-ami" {
 *   source  = "../../tf-modules/ami-ubuntu"
 *   release = "14.04"
 * }
 * ```
 *
 * To use the AMI on EC2, reference it by ID like this: `${module.ubuntu-xenial-ami.id}`
 *
 * The module will filter the AMI by the following criteria:
 *
 * * Canonical's account ARN (for either corporate or govcloud partitions)
 * * the most recent release
 * * hvm-type AMIs
 * * amd64
 * * the `name` filter
 *
 * The `name` filter looks like:
 *
 * ```
 * filter {
     name   = "name"
     values = ["ubuntu/images/hvm-ssd/ubuntu-${var.name_map[var.release]}-${var.release}-amd64-server-*"]
 * }
 * ```
 *
 * If you deploy an instance with this AMI, and later do a `terraform plan`, the
 * most recent AMI will be looked up, and that may change the module output, and
 * then the AMI for the instance. You can use `ignore_changes` or `-target`, to
 * work around that situation, or take that as your reminder to replace the
 * instance with a more recent release of the upstream AMI.
 *
 */

variable "most_recent" {
  default     = true
  description = "boolean, maps to `most_recent` parameter for `aws_ami` data source"
}

variable "name_map" {
  default = {
    "17.10" = "artful"
    "17.04" = "zesty"
    "16.04" = "xenial"
    "14.04" = "trusty"
  }

  description = "map of release numbers to names, including trusty, xenial, zesty, and artful"
}

variable "release" {
  default     = "16.04"
  description = "default ubuntu release to target"
}

variable "is_govcloud" {
  default     = false
  description = "boolean, switch between Canonical's account ARN in `aws_ami.owners`"
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
