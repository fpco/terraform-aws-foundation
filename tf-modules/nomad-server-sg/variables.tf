variable "name" {
    default = "test"
    description = "A short identifier to use in the name of the security group"
}
variable "vpc_id" {
    description = "The ID of the VPC to deploy to"
}
variable "server_cidr_blocks" {
    description = "The list of CIDR IP blocks where nomad servers run (as a string-list)"
}
variable "worker_cidr_blocks" {
    description = "The list of CIDR IP blocks where nomad agents run (as a string-list)"
}

