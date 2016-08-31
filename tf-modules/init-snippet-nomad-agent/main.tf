resource "template_file" "init-snippet" {
    template = "${path.module}/snippet.tpl"
    vars {
        nomad_pillar = "${var.nomad_pillar}"
        init_prefix = "${var.init_prefix}"
        init_suffix = "${var.init_suffix}"
        log_prefix = "${var.log_prefix}"
        log_level = "${var.log_level}"
        bootstrap_pillar_file = "${var.bootstrap_pillar_file}"
    }
}
output "init_snippet" {
    value = "${template_file.init-snippet.rendered}"
}
