variable "bootstrap_pillar_file" {
  default     = "/srv/pillar/bootstrap.sls"
  description = "path, to the 'bootstrap' pillar file"
  type = string
}

variable "consul_secret_key" {
  description = "Secret key provided to consul, for cluster crypto"
  type = string
}

variable "consul_client_token" {
  description = "Client token for services on the node, connecting to consul as a client"
  type = string
}

variable "init_prefix" {
  default     = ""
  description = "initial init (shellcode) to prefix this snippet with"
  type = string
}

variable "init_suffix" {
  default     = ""
  description = "init (shellcode) to append to the end of this snippet"
  type = string
}

variable "disable_consul_remote_exec" {
  default     = true
  description = "True/False boolean pillar key (note case) for the consul salt formula"
  type = string
}

variable "datacenter" {
  description = "consul config 'datacenter'"
  type = string
}

variable "leader_dns" {
  description = "DNS to Consul leaders"
  type = string
}

variable "log_level" {
  default     = "info"
  description = "set log verbosity on node init, for CM with saltstack"
  type = string
}

variable "log_prefix" {
  default     = "OPS:"
  description = "string to prefix log messages with"
  type = string
}

variable "retry_interval" {
  default     = "1s"
  description = "passed to the consul agent, period to pause between attempts to join"
  type = string
}

variable "consul_webui" {
  default     = false
  description = "True/False boolean (note case) that maps to enable the webui via the consul:webui pillar key"
  type = bool
}

