variable "cidr_prefix_a" {
  default     = ""
  description = "CIDR block prefix to seed IP details in the formula, subnet a"
  type = string
}

variable "cidr_prefix_c" {
  default     = ""
  description = "CIDR block prefix to seed IP details in the formula, subnet c"
  type = string
}

variable "consul_secret_key" {
  default     = "FOOBAR=="
  description = "Secret key provided to consul, for cluster crypto"
  type = string
}

variable "consul_client_token" {
  default     = "UUID"
  description = "Client token for services on the node, connecting to consul as a client"
  type = string
}

variable "consul_master_token" {
  default     = "UUID"
  description = "Master token provided to consul leader as root ACL/token"
  type = string
}

variable "consul_webui" {
  default     = false
  description = "True/False boolean (note case) that maps to enable the webui via the consul:webui pillar key"
  type = bool
}

variable "leader_count" {
  default     = "3"
  description = "Number of leaders to bootstral consul cluster"
  type = number
}

variable "datacenter" {
  default     = ""
  description = "Provided to consul as datacenter config, usual maps to AWS region"
  type = string
}

variable "disable_consul_remote_exec" {
  default     = true
  description = "True/False boolean pillar key (note case) for the consul salt formula"
  type = bool
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

variable "hostname_prefix" {
  default     = "leaders"
  description = "when we update the hostname, prefix i-xxxxxx with this"
  type = string
}

variable "log_level" {
  default     = "debug"
  description = "set log verbosity on node init, for CM with saltstack"
  type = string
}

