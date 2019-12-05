variable "bootstrap_pillar_file" {
  default     = "/srv/pillar/bootstrap.sls"
  description = "path, to the 'bootstrap' pillar file"
  type        = string
}

variable "cidr_prefix_a" {
  description = "CIDR block prefix to seed IP details in the formula, subnet a"
  type = string
}

variable "cidr_prefix_c" {
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

variable "leader_count" {
  default     = "3"
  description = "Number of leaders to bootstral consul cluster"
  type = number
}

variable "datacenter" {
  description = "Provided to consul as datacenter config, usual maps to AWS region"
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

variable "log_prefix" {
  default     = "OPS:"
  description = "string to prefix log messages with"
  type = string
}

