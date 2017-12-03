/**
 * ## Init Snippet: Write Bootstrap Pillar
 *
 * Document.
 *
 */

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

variable "pillar" {
  description = "salt pillar to write out to bootstrap.sls"
}

data "template_file" "init_snippet" {
  template = "${file("${path.module}/snippet.tpl")}"

  vars {
    bootstrap_pillar_file = "${var.bootstrap_pillar_file}"
    init_prefix           = "${var.init_prefix}"
    init_suffix           = "${var.init_suffix}"
    pillar                = "${var.pillar}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
