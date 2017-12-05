/**
 * ## Generic Host Init for Consul Agents
 * 
 * The purpose of this module is to provide the `user_data` to init a new Host that
 * will be joining a network or SOA cluster built on Consul.
 * 
 * This module relies on the same assumptions as the other modules in this
 * repository (we're using saltstack for CM and to abstract away the details). Thus,
 * the `user_data` generated makes use of Saltstack, the platform's configuration
 * management formula and related abstractions.
 * 
 * The goals of this module are:
 * 
 * * make the code easier to read and maintain,
 * * make the ASG definitions more reliable and consistent,
 * * simplify the code that defines the environment as a whole
 * 
 * NOTE: need to update for template_file data type
 * 
 * ### Example
 * 
 * ```
 * # provisioning for worker cluster
 * module "worker-init" {
 *     source = "../tf-modules/consul-agent-generic-init"
 *     datacenter = "${var.datacenter}"
 *     service = "worker"
 *     consul_secret_key = "${var.consul_secret_key}"
 *     consul_client_token = "${var.consul_master_token}"
 *     leader_dns = "${module.consul-leaders.leader_dns}"
 *     extra_pillar = "extra: pillar"
 *     extra_init = <<EOF
 * echo "customize this node's init.."
 * date
 * consul --version
 * salt-call --version
 * uname -a
 * EOF
 * }
 * ```
 */

# render init script for a cluster using our generic template
resource "template_file" "generic_init" {
  template = "${path.module}/init.tpl"

  vars {
    extra_init          = "${var.extra_init}"
    extra_pillar        = "${var.extra_pillar}"
    consul_secret_key   = "${var.consul_secret_key}"
    consul_client_token = "${var.consul_client_token}"
    consul_webui        = "${var.consul_webui}"
    disable_remote_exec = "${var.disable_consul_remote_exec}"
    leader_dns          = "${var.leader_dns}"
    log_level           = "${var.log_level}"
    datacenter          = "${var.datacenter}"
    service             = "${var.service}"
    retry_interval      = "${var.retry_interval}"
  }
}
