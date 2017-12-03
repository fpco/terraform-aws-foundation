/**
 * ## Init Snippet: Config Consul Leader
 *
 * Without CM/SaltStack/Ansible/etc, just render a config file. Pretty static,
 * very simple, but good enough for basic demos, proofs, and tests.
 *
 */

# variables used by this snippet of init shellcode
variable "encrypt" {
  description = "base64 encoded key for consul gossip encryption"
}

variable "data_dir" {
  default     = "/opt/consul"
  description = "path on consul server for -data-dir"
}

variable "datacenter" {
  description = "name of consul datacenter, maps to -datacenter config"
}

variable "config_file" {
  default     = "/etc/consul.json"
  description = "path to consul config file"
}

variable "init_prefix" {
  default     = ""
  description = "initial init (shellcode) to prefix this snippet with"
}

variable "init_suffix" {
  default     = ""
  description = "init (shellcode) to append to the end of this snippet"
}

variable "log_prefix" {
  default     = "OPS: "
  description = "string to prefix log messages with"
}

# render init script snippet from the template
data "template_file" "init_snippet" {
  template = "${file("${path.module}/snippet.tpl")}"

  vars {
    encrypt     = "${var.encrypt}"
    data_dir    = "${var.data_dir}"
    datacenter  = "${var.datacenter}"
    config_file = "${var.config_file}"
    init_prefix = "${var.init_prefix}"
    init_suffix = "${var.init_suffix}"
    log_prefix  = "${var.log_prefix}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
