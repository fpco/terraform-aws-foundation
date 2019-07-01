/**
 * ## Init Snippet: Hostname (simple shell version)
 *
 * Generate an init snippet that uses `hostnamectl` to update the hostname.
 * The hostname is also updated in `/etc/hosts`. The hostname is derived from
 * the instance ID and the `hostname_prefix` variable, and will look like:
 * `web-server-i-101abd22`
 *
 */

# variables used by this snippet of init shellcode
variable "init_prefix" {
  default     = ""
  description = "initial init (shellcode) to prefix this snippet with"
  type = string
}

variable "init_suffix" {
  default     = ""
  description = "init (shellcode) to append to the end of this snippet"
  type = string
}

variable "hostname_prefix" {
  description = "role, name, or term to use as a prefix to the AWS-style hostname"
  type = string
}

variable "log_prefix" {
  default     = "OPS:"
  description = "string to prefix log messages with"
  type = string
}

# render init script for a cluster using our generic template
data "template_file" "init_snippet" {
  template = file("${path.module}/snippet.tpl")

  vars = {
    hostname_prefix = var.hostname_prefix
    init_prefix     = var.init_prefix
    init_suffix     = var.init_suffix
    log_prefix      = var.log_prefix
  }
}

output "init_snippet" {
  value = data.template_file.init_snippet.rendered
}

