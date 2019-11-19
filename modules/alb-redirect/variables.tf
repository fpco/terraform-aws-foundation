variable "lb_arn" {
  type        = string
  description = "The ARN of the ALB to listen."
}

variable "http_port" {
  type        = number
  description = "The uncrypted web service port listened on ALB. No service should actually servicing on this port."
}

variable "https_port" {
  type        = number
  description = "The crypted web service port listened on ALB."
}
