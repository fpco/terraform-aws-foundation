/**
 * ## Init Snippet: Hostname Management (SaltStack version)
 *
 * Generage an init snippet that does the following:
 *
 * * lookup the instance id and derive the new hostname
 * * write the new hostname to `/etc/salt/minion_id`
 * * write the new hostname to bootstrap pillar
 * * use `salt-call` to apply the `hostname` formula from `fpco-salt-formula`.
 *   This formula will set the hostname and refresh salt so it's minion ID is
 *   correct in the cache and elsewhere.
 *
 * The hostname is derived from the instance ID and the `hostname_prefix`
 * variable, and will look like: `web-server-i-101abd22`.
 *
 */

# variables used by this snippet of init shellcode
variable "bootstrap_pillar_file" {
  default     = "/srv/pillar/bootstrap.sls"
  description = "path, to the 'bootstrap' pillar file"
}

variable "init_prefix" {
  default     = ""
  description = "initial init (shellcode) to prefix this snippet with"
}

variable "init_suffix" {
  default     = ""
  description = "init (shellcode) to append to the end of this snippet"
}

variable "hostname_prefix" {
  description = "role, name, or term to use as a prefix to the AWS-style hostname"
}

variable "log_level" {
  default     = "info"
  description = "info, debug, etc the 'log level' to specify to salt/etc"
}

variable "log_prefix" {
  default     = "OPS:"
  description = "string to prefix log messages with"
}

# render init script for a cluster using our generic template
data "template_file" "init_snippet" {
  template = "${file("${path.module}/snippet.tpl")}"

  vars {
    bootstrap_pillar_file = "${var.bootstrap_pillar_file}"
    hostname_prefix       = "${var.hostname_prefix}"
    init_prefix           = "${var.init_prefix}"
    init_suffix           = "${var.init_suffix}"
    log_level             = "${var.log_level}"
    log_prefix            = "${var.log_prefix}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
