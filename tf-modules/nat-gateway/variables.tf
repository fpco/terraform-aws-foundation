variable "region" {
    default = ""
    description = "The AWS region"
}
variable "access_key" {
    description = "AWS key id"
}
variable "secret_key" {
    description = "AWS key secret"
}
variable "subnet_id" {
    description = "The Subnet ID of the public subnet in which to place the gateway."
}
