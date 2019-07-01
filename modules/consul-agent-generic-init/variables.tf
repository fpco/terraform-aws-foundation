variable "service" {
  default     = "agent"
  description = "Primary role serviced by nodes in this cluster"
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

variable "consul_webui" {
  default     = false
  description = "True/False boolean (note case) that maps to enable the webui via the consul:webui pillar key"
  type = bool
}

variable "leader_dns" {
  description = "DNS to find consul leaders to follow/join"
  type = string
}

variable "datacenter" {
  description = "The AWS region, provided to consul as datacenter"
  type = string
}

variable "extra_pillar" {
  default     = ""
  description = "YAML to insert as pillar in bootstrap.sls"
  type = string
}

variable "extra_init" {
  default     = ""
  description = "shell/bash to append to node init via user_data"
  type = string
}

variable "disable_consul_remote_exec" {
  default     = true
  description = "True/False boolean pillar key (note case) for the consul salt formula"
  type = bool
}

variable "log_level" {
  default     = "info"
  description = "set log verbosity on node init, for CM with saltstack"
  type = string
}

variable "retry_interval" {
  default     = "1s"
  description = "passed to the consul agent, period to pause between attempts to join"
  type = string
}

