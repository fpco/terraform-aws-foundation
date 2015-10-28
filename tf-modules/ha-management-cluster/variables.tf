variable "name" {
    default = ""
    description = "Name of the management cluster"
}
variable "vpc_id" {
    default = ""
    description = "ID of the management cluster's AWS VPC to deploy to"
}
variable "worker_cidr_blocks" {
    description = "List of CIDR IP blocks with access to the management services (string)"
}
variable "consul_secret_key" {
    default = ""
    description = ""
}
variable "consul_master_token" {
    default = ""
    description = ""
}
variable "leader_dns" {
    default = ""
    description = ""
}
variable "region" {
    default = ""
    description = "The AWS region, provided to consul as datacenter"
}
variable "access_key" {
    description = "AWS key id"
}
variable "secret_key" {
    description = "AWS key secret"
}
