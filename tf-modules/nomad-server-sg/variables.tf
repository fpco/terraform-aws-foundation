variable "name" {
  default     = "test"
  description = "A short identifier to use in the name of the security group"
}

variable "vpc_id" {
  description = "The ID of the VPC to deploy to"
}

variable "server_cidr_blocks" {
  description = "The list of CIDR IP blocks where nomad servers run"
  type        = "list"
}

variable "worker_cidr_blocks" {
  description = "The list of CIDR IP blocks where nomad workers run"
  type        = "list"
}
