/**
 * ## Init Snippet: Consul Leader
 *
 * Configure and run the consul service as a leader, using the `consul.service`
 * Salt formula from `fpco-salt-formula`.
 *
 * This will write out bootstrap pillar before applying the formula. This is
 * based on the DNS method for discovering the leaders, and should be updated
 * to leverage the AWS API for that discovery.
 *
 */

data "template_file" "init_snippet" {
  template = "${file("${path.module}/snippet.tpl")}"

  vars {
    cidr_prefix_a       = "${var.cidr_prefix_a}"
    cidr_prefix_c       = "${var.cidr_prefix_c}"
    client_token        = "${var.consul_client_token}"
    consul_webui        = "${var.consul_webui}"
    datacenter          = "${var.datacenter}"
    disable_remote_exec = "${var.disable_consul_remote_exec}"
    init_prefix         = "${var.init_prefix}"
    init_suffix         = "${var.init_suffix}"
    leader_count        = "${var.leader_count}"
    log_prefix          = "${var.log_prefix}"
    log_level           = "${var.log_level}"
    master_token        = "${var.consul_client_token}"
    secret_key          = "${var.consul_secret_key}"
  }
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
