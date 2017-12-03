/**
 * ## Init Snippet: Nomad Agent
 *
 * Generate an init snippet to configure and run the `nomad` agent
 * using SaltStack:
 *
 * * write out bootstrap pillar
 * * use `salt-call` to apply the `nomad.service` formula from
 *   `fpco-salt-formula`
 *
 */

data "template_file" "init_snippet" {
  template = "${file("${path.module}/snippet.tpl")}"

  vars {
    nomad_pillar          = "${var.nomad_pillar}"
    init_prefix           = "${var.init_prefix}"
    init_suffix           = "${var.init_suffix}"
    log_prefix            = "${var.log_prefix}"
    log_level             = "${var.log_level}"
    bootstrap_pillar_file = "${var.bootstrap_pillar_file}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
