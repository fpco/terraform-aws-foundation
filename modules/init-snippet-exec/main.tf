/**
 * ## Init Snippet: Shell Exec
 *
 * This module is a bit more of a simple pass-through wrapper for the
 * `template_file` resource so we maintain the same code-flow with "random"
 * shell script for init snippets as we do our other "boxed" snippets.
 *
 */

variable "init" {
  description = "init shellcode for this snippet"
}

data "template_file" "init_snippet" {
  template = "${file("${path.module}/snippet.tpl")}"

  vars {
    init = "${var.init}"
  }
}

// Rendered "init snippet" from the template file
output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
