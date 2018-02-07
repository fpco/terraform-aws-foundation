/**
 * ## Init Snippet: Install AWS CLI Tool
 *
 * Generate an init snippet to install the AWS cli tool on boot. This will:
 *
 * * use apt to install `python-pip`
 * * use `pip` to upgrade `pip`
 * * us `pip` to install `awscli`
 *
 */

# variables used by this snippet of init shellcode
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
    init_prefix = "${var.init_prefix}"
    init_suffix = "${var.init_suffix}"
    log_prefix  = "${var.log_prefix}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
