variable "lb_arn" {
  type        = string
  description = "The ARN of the ALB to listen."
}

variable "lb_port" {
  type        = number
  description = "The port listened on ALB."
}

variable "protocol" {
  type        = string
  description = "Either HTTP or HTTPS."
}

variable "https_cert_arn" {
  type        = string
  default     = ""
  description = "The ARN of the cert for HTTPS. Required if protocol is HTTPS."
}

variable "ssl_policy" {
  type        = string
  default     = "ELBSecurityPolicy-2016-08"
  description = "The name of the SSL Policy for the listener. Required if protocol is HTTPS."
}

variable "service_port" {
  type        = number
  description = "The port listened on service."
}

variable "vpc_id" {
  type        = string
  description = "The identifier of the VPC in which to create the target groups."
}

variable "tags" {
  type        = map(string)
  default     = {}
  description = "Tags for aws_lb resource."
}

variable "name_prefix" {
  type = string
}
