variable "region" {
    default = ""
    description = "The AWS region"
}
variable "access_key" {
    description = "AWS key id"
    default = ""
}
variable "secret_key" {
    description = "AWS key secret"
    default = ""
}
variable "subnet_id" {
    description = "The Subnet ID of the public subnet in which to place the gateway."
}
