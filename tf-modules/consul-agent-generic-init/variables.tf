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
variable "log_level" {
    default = "debug"
    description = "set log verbosity on node init, for CM with saltstack"
}
