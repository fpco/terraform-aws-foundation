variable "subnet_ids" {
  description = "Public subnet IDs where to place the gateways"
  type = "list"
}

variable "nat_count" {
  description = "How many NAT gateways to setup"
}
