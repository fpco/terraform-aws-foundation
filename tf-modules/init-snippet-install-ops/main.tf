/**
 * ## Init Snippet: Install Ops
 *
 * Use shell to install the `ops` cli tool from FP Complete's S3 bucket.
 *
 */

# variables used by this snippet of init shellcode
variable "version" {
  default     = "0.3.2"
  description = "version of ops, to install"
}

variable "base_url" {
  default     = "https://download.fpcomplete.com/ops"
  description = "base of the download URL to ops executable"
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
    version     = "${var.version}"
    base_url    = "${var.base_url}"
    init_prefix = "${var.init_prefix}"
    init_suffix = "${var.init_suffix}"
    log_prefix  = "${var.log_prefix}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
