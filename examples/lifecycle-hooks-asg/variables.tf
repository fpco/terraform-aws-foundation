variable "count" {
 default = 1
}

variable "region" {
 description = "AWS region for hosting our your network"
 default = "ap-south-1"
}

variable "ssh_pubkey" {
  description = "File path to SSH public key"
  default     = "./id_rsa.pub"
}

variable "ssh_key" {
  description = "File path to SSH private key"
  default     = "./id_rsa"
}

variable "amis" {
 description = "Base AMI to launch the instances"
 default = {
 ap-south-1 = "ami-8da8d2e2"
 }
}
