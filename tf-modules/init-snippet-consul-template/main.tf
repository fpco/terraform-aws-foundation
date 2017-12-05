/**
 * ## Init Snippet: Consul Template
 *
 * Configure and run `consul-template` as a system service using the
 * `consul.template-tool.service` formula from `fpco-salt-formula`. This will
 * write out bootstrap pillar before applying the formula.
 *
 */

variable "bootstrap_pillar_file" {
  default     = "/srv/pillar/bootstrap.sls"
  description = "path, to the 'bootstrap' pillar file"
}

variable "consul_addr" {
  default     = "127.0.0.1:8500"
  description = "Address to consul, in the form host:port"
}

variable "consul_client_token" {
  description = "Client token for services on the node, connecting to consul as a client"
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

data "template_file" "init_snippet" {
  template = "${file("${path.module}/snippet.tpl")}"

  vars {
    bootstrap_pillar_file = "${var.bootstrap_pillar_file}"
    consul_addr           = "${var.consul_addr}"
    consul_client_token   = "${var.consul_client_token}"
    init_prefix           = "${var.init_prefix}"
    init_suffix           = "${var.init_suffix}"
    log_prefix            = "${var.log_prefix}"
    log_level             = "${var.log_level}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
