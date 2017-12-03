/**
 * ## Init Snippet: Shell Exec
 *
 * Document.
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

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
