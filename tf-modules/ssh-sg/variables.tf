variable "name" {
    description = "The name prefix for this SSH security group"
}
variable "allowed_cidr_blocks" {
    description = "The CIDR block to allow access to"
    default     = ["0.0.0.0/0"]
}
variable "vpc_id" {
    description = "The ID of the VPC to deploy to"
}
