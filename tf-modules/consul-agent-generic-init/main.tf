# render init script for a cluster using our generic template
resource "template_file" "generic_init" {
    filename = "${path.module}/init.tpl"
    vars {
        extra_init = "${var.extra_init}"
        extra_pillar = "${var.extra_pillar}"
        consul_secret_key = "${var.consul_secret_key}"
        consul_client_token = "${var.consul_client_token}"
        leader_dns = "${var.leader_dns}"
        log_level = "${var.log_level}"
        region = "${var.region}"
        service = "${var.service}"
    }
}
