variable "name_prefix" {
  description = "Prefix that will be added to names of all resources"
  type        = string
}

variable "name_suffix" {
  description = "suffix to include when naming the various resources"
  default     = "kube-load-balancer"
  type        = string
}

variable "vpc_id" {
  description = "VPC id for the security group"
  type        = string
}

variable "api_port" {
  description = "TCP port the load balancer should be configured to listen on"
  default     = "443"
  type        = string
}

variable "cidr_blocks_api" {
  description = "list of CIDR blocks that should have access to the kube API"
  type        = list(string)
  default     = ["0.0.0.0/0"]
}

