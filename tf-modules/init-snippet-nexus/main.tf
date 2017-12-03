/**
 * ## Init Snippet: Run Nexus (w/ Docker)
 *
 * Use Docker to run the Nexus package manager.
 * DOCUMENT.
 *
 */

variable "device_path" {
  default     = "/dev/xvdf"
  description = "path, to the device's path in /dev/"
}

variable "device_label" {
  default     = "nexus"
  description = "label of the device's volume"
}

variable "mount_path" {
  description = "Where to mount the device"
  default     = "/nexus"
}

variable "data_path" {
  description = "Where to put the nexus data"
  default     = "/nexus/data"
}

variable "port" {
  description = "The nexus port to expose"
  default     = "8081"
}

data "template_file" "init_snippet" {
  template = "${file("${path.module}/snippet.tpl")}"

  vars {
    device_path  = "${var.device_path}"
    device_label = "${var.device_label}"

    mount_path = "${var.mount_path}"
    data_path  = "${var.data_path}"
    port       = "${var.port}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
