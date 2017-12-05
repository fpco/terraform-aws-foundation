/**
 * ## Init Snippet: Config Upstart, run Consul
 *
 * Generate an init snippet that will configure Upstart to run `consul` as a
 * system service.
 *
 */

# variables used by this snippet of init shellcode
variable "upstart_config" {
  default     = "/etc/init/consul.conf"
  description = "file path to upstart config"
}

variable "consul_user" {
  default     = "consul"
  description = "user to run consul as"
}

variable "consul_bin" {
  default     = "/usr/local/bin/consul"
  description = "path to consul executable"
}

variable "config_file" {
  default     = "/etc/consul.json"
  description = "path to consul config file"
}

variable "config_dir" {
  default     = "/etc/consul.d"
  description = "path for consul configs (.d directory)"
}

variable "init_prefix" {
  default     = ""
  description = "initial init (shellcode) to prefix this snippet with"
}

variable "init_suffix" {
  default     = ""
  description = "init (shellcode) to append to the end of this snippet"
}

# render init script snippet from the template
data "template_file" "init_snippet" {
  template = "${file("${path.module}/snippet.tpl")}"

  vars {
    upstart_config = "${var.upstart_config}"
    consul_user    = "${var.consul_user}"
    config_file    = "${var.config_file}"
    config_dir     = "${var.config_dir}"
    consul_bin     = "${var.consul_bin}"
    init_prefix    = "${var.init_prefix}"
    init_suffix    = "${var.init_suffix}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
