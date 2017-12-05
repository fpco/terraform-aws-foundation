/**
 * ## Init Snippet: Run Prometheus
 *
 * Generate an init snippet to configure and run `prometheus` as a system
 * service using SaltStack:
 *
 * * write out bootstrap pillar
 * * use `salt-call` to apply the `prometheus.server` formula from
 *   `fpco-salt-formula`
 *
 */

variable "bootstrap_pillar_file" {
  default     = "/srv/pillar/bootstrap.sls"
  description = "path, to the 'bootstrap' pillar file"
}

variable "prometheus_pillar" {
  description = "salt pillar for prometheus, sent to bootstrap.sls"
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
    prometheus_pillar     = "${var.prometheus_pillar}"
    init_prefix           = "${var.init_prefix}"
    init_suffix           = "${var.init_suffix}"
    log_prefix            = "${var.log_prefix}"
    log_level             = "${var.log_level}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
