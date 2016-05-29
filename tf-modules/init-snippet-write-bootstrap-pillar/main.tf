variable "bootstrap_pillar_file" {
    default = "/srv/pillar/bootstrap.sls"
    description = "path, to the 'bootstrap' pillar file"
}
variable "init_prefix" {
    default = ""
    description = "initial init (shellcode) to prefix this snippet with"
}
variable "init_suffix" {
    default = ""
    description = "init (shellcode) to append to the end of this snippet"
}
variable "pillar" {
    description = "salt pillar to write out to bootstrap.sls"
}
resource "template_file" "init-snippet" {
    template = "${path.module}/init.tpl"
    vars {
        bootstrap_pillar_file = "${var.bootstrap_pillar_file}"
        init_prefix = "${var.init_prefix}"
        init_suffix = "${var.init_suffix}"
        pillar = "${var.pillar}"
    }
}
output "init_snippet" {
    value = "${template_file.init-snippet.rendered}"
}
