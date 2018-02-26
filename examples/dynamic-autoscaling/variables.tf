variable "region" {
  default = "eu-west-1"
}

variable "name" {
  default = "demo-dynamic-autoscaling"
}

variable "ssh_pubkey" {
  default     = "./id_rsa.pub"
  description = "The path to the SSH pub key to use"
}

variable "ssh_key" {
  default     = "./id_rsa"
  description = "The path to the SSH key to use"
}

variable "vpc" {
  default = {
    "cidr" = "10.0.0.0/16"
  }
}
