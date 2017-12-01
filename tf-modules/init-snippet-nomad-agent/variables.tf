variable "bootstrap_pillar_file" {
  default     = "/srv/pillar/bootstrap.sls"
  description = "path, to the 'bootstrap' pillar file"
}

variable "nomad_pillar" {
  description = "pillar to pass to bootstrap.sls, for nomad.service formula"
}

variable "init_prefix" {
  default     = ""
  description = "initial init (shellcode) to prefix this snippet with"
}

variable "init_suffix" {
  default     = ""
  description = "init (shellcode) to append to the end of this snippet"
}

variable "log_level" {
  default     = "info"
  description = "set log verbosity on node init, for CM with saltstack"
}

variable "log_prefix" {
  default     = "OPS:"
  description = "string to prefix log messages with"
}
