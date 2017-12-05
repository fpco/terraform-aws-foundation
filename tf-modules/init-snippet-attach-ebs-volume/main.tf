/**
 * ## Init Snippet: Attach EBS Volume
 *
 * Create an init snippet that will attach an EBS volume to the instance.
 * This snippet requires that the instance has an IAM instance profile which
 * grants it the access needed to find and attach the EBS volume. There are
 * other modules in this repo which can create EBS volumes with IAM profiles
 * for each volume. Attaching the EBS volume will loop until it succeeds.
 *
 */

# variables used by this snippet of init shellcode
variable "device_path" {
  default     = "/dev/xvdf"
  description = "path, to the device's path in /dev/"
}

variable "init_prefix" {
  default     = ""
  description = "initial init (shellcode) to prefix this snippet with"
}

variable "init_suffix" {
  default     = ""
  description = "init (shellcode) to append to the end of this snippet"
}

variable "log_level" {
  default     = "info"
  description = "default log level verbosity for apps that support it"
}

variable "log_prefix" {
  default     = "OPS: "
  description = "string to prefix log messages with"
}

variable "region" {
  description = "AWS region the volume is in"
}

variable "wait_interval" {
  default     = "5"
  description = "time (in seconds) to wait when looping to find the device"
}

variable "volume_id" {
  description = "ID of the EBS volume to attach"
}

# render init script for a cluster using our generic template
data "template_file" "init_snippet" {
  template = "${file("${path.module}/snippet.tpl")}"

  vars {
    device_path   = "${var.device_path}"
    init_prefix   = "${var.init_prefix}"
    init_suffix   = "${var.init_suffix}"
    log_prefix    = "${var.log_prefix}"
    log_level     = "${var.log_level}"
    region        = "${var.region}"
    volume_id     = "${var.volume_id}"
    wait_interval = "${var.wait_interval}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
