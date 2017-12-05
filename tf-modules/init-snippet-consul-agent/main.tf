/**
 * ## Init Snippet: Consul Agent
 *
 * Configure and run the consul agent using the `consul.service` Salt formula
 * from `fpco-salt-formula`.
 *
 */

data "template_file" "init_snippet" {
  template = "${file("${path.module}/snippet.tpl")}"

  vars {
    bootstrap_pillar_file = "${var.bootstrap_pillar_file}"
    consul_secret_key     = "${var.consul_secret_key}"
    consul_client_token   = "${var.consul_client_token}"
    consul_webui          = "${var.consul_webui}"
    datacenter            = "${var.datacenter}"
    disable_remote_exec   = "${var.disable_consul_remote_exec}"
    init_prefix           = "${var.init_prefix}"
    init_suffix           = "${var.init_suffix}"
    leader_dns            = "${var.leader_dns}"
    log_prefix            = "${var.log_prefix}"
    log_level             = "${var.log_level}"
    retry_interval        = "${var.retry_interval}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
