output "user_data" {
  value       = templatefile("${path.module}/init.tpl", {
    extra_init          = var.extra_init
    extra_pillar        = var.extra_pillar
    consul_secret_key   = var.consul_secret_key
    consul_client_token = var.consul_client_token
    consul_webui        = var.consul_webui
    disable_remote_exec = var.disable_consul_remote_exec
    leader_dns          = var.leader_dns
    log_level           = var.log_level
    datacenter          = var.datacenter
    service             = var.service
    retry_interval      = var.retry_interval
  })
  description = "The template, rendered"
}

