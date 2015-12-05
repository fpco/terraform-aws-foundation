variable "service" {
    default = "agent"
    description = "Primary role serviced by nodes in this cluster"
}
variable "consul_secret_key" {
    default = "FOOBAR=="
    description = "Secret key provided to consul, for cluster crypto"
}
variable "consul_client_token" {
    default = "UUID"
    description = "Client token for services on the node, connecting to consul as a client"
}
variable "consul_webui" {
    default = "False"
    description = "True/False boolean (note case) that maps to enable the webui via the consul:webui pillar key"
}
variable "leader_dns" {
    default = ""
    description = "DNS to find consul leaders to follow/join"
}
variable "region" {
    default = ""
    description = "The AWS region, provided to consul as datacenter"
}
variable "extra_pillar" {
    default = ""
    description = "YAML to insert as pillar in bootstrap.sls"
}
variable "extra_init" {
    default = ""
    description = "shell/bash to append to node init via user_data"
}
variable "disable_consul_remote_exec" {
    default = "True"
    description = "True/False boolean pillar key (note case) for the consul salt formula"
}
variable "log_level" {
    default = "debug"
    description = "set log verbosity on node init, for CM with saltstack"
}
