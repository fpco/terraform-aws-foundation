variable "cidr_prefix_a" {
  description = "CIDR block prefix to seed IP details in the formula, subnet a"
}

variable "cidr_prefix_c" {
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

variable "init_prefix" {
  default     = ""
  description = "initial init (shellcode) to prefix this snippet with"
}

variable "init_suffix" {
  default     = ""
  description = "init (shellcode) to append to the end of this snippet"
}

variable "leader_count" {
  default     = "3"
  description = "Number of leaders to bootstral consul cluster"
}

variable "datacenter" {
  description = "Provided to consul as datacenter config, usual maps to AWS region"
}

variable "disable_consul_remote_exec" {
  default     = "True"
  description = "True/False boolean pillar key (note case) for the consul salt formula"
}

variable "log_level" {
  default     = "info"
  description = "set log verbosity on node init, for CM with saltstack"
}

variable "log_prefix" {
  default     = "OPS:"
  description = "string to prefix log messages with"
}
