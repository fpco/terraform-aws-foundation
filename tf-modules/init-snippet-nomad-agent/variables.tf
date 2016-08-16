variable "bootstrap_pillar_file" {
    default = "/srv/pillar/bootstrap.sls"
    description = "path, to the 'bootstrap' pillar file"
}
variable "nomad_secret" {
    description = "Secret key provided to nomad, maps to 'secret' in config"
}
variable "consul_client_token" {
    description = "Client token to use when connecting to consul as a client"
}
variable "init_prefix" {
    default = ""
    description = "initial init (shellcode) to prefix this snippet with"
}
variable "init_suffix" {
    default = ""
    description = "init (shellcode) to append to the end of this snippet"
}
variable "enable_raw_exec" {
    default = "True"
    description = "True/False boolean pillar key (note case) maps to nomad raw_exec"
}
variable "name" {
    description = "name goes into 'datacenter' config"
}
variable "log_level" {
    default = "info"
    description = "set log verbosity on node init, for CM with saltstack"
}
variable "log_prefix" {
    default = "OPS:"
    description = "string to prefix log messages with"
}
variable "region" {
    description = "suffix in datacenter config, as in name.region"
}
variable "server" {
    default = "False"
    description = "True/False boolean (note case) to run nomad in server mode"
}
