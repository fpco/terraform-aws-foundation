variable "cidr_prefix_a" {
  default     = ""
  description = "CIDR block prefix to seed IP details in the formula, subnet a"
}

variable "cidr_prefix_c" {
  default     = ""
  description = "CIDR block prefix to seed IP details in the formula, subnet c"
}

variable "consul_secret_key" {
  default     = "FOOBAR=="
  description = "Secret key provided to consul, for cluster crypto"
}

variable "consul_client_token" {
  default     = "UUID"
  description = "Client token for services on the node, connecting to consul as a client"
}

variable "consul_master_token" {
  default     = "UUID"
  description = "Master token provided to consul leader as root ACL/token"
}

variable "consul_webui" {
  default     = "False"
  description = "True/False boolean (note case) that maps to enable the webui via the consul:webui pillar key"
}

variable "leader_count" {
  default     = "3"
  description = "Number of leaders to bootstral consul cluster"
}

variable "datacenter" {
  default     = ""
  description = "Provided to consul as datacenter config, usual maps to AWS region"
}

variable "disable_consul_remote_exec" {
  default     = "True"
  description = "True/False boolean pillar key (note case) for the consul salt formula"
}

variable "extra_pillar" {
  default     = ""
  description = "YAML to insert as pillar in bootstrap.sls"
}

variable "extra_init" {
  default     = ""
  description = "shell/bash to append to node init via user_data"
}

variable "hostname_prefix" {
  default     = "leaders"
  description = "when we update the hostname, prefix i-xxxxxx with this"
}

variable "log_level" {
  default     = "debug"
  description = "set log verbosity on node init, for CM with saltstack"
}
