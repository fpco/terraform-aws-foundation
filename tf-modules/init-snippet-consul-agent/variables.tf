variable "bootstrap_pillar_file" {
  default     = "/srv/pillar/bootstrap.sls"
  description = "path, to the 'bootstrap' pillar file"
}

variable "consul_secret_key" {
  description = "Secret key provided to consul, for cluster crypto"
}

variable "consul_client_token" {
  description = "Client token for services on the node, connecting to consul as a client"
}

variable "init_prefix" {
  default     = ""
  description = "initial init (shellcode) to prefix this snippet with"
}

variable "init_suffix" {
  default     = ""
  description = "init (shellcode) to append to the end of this snippet"
}

variable "disable_consul_remote_exec" {
  default     = "True"
  description = "True/False boolean pillar key (note case) for the consul salt formula"
}

variable "datacenter" {
  description = "consul config 'datacenter'"
}

variable "leader_dns" {
  description = "DNS to Consul leaders"
}

variable "log_level" {
  default     = "info"
  description = "set log verbosity on node init, for CM with saltstack"
}

variable "log_prefix" {
  default     = "OPS:"
  description = "string to prefix log messages with"
}

variable "retry_interval" {
  default     = "1s"
  description = "passed to the consul agent, period to pause between attempts to join"
}

variable "consul_webui" {
  default     = "False"
  description = "True/False boolean (note case) that maps to enable the webui via the consul:webui pillar key"
}
