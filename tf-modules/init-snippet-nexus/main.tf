/**
 * ## Init Snippet: Run Nexus (w/ Docker)
 *
 * Generate an init snippet to use Docker to run the Nexus package manager:
 *
 * * use `apt-get` to install docker
 * * create `/etc/rc.local` to create/chown the `data_path` and run the
 *   docker image
 *
 * The `sonatype/nexus` docker image is run.
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
