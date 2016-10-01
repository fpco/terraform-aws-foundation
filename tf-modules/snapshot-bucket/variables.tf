variable "name" {
    description = "Name to give bucket"
}
variable "env" {
    description = "Environment associated with bucket"
}
variable "access_key" {
    description = "AWS access key"
    default = ""
}
variable "secret_key" {
     description = "AWS secret access key"
    default = ""
}
variable "region"     {
    default     = "us-west-1"
    description = "AWS region to host your network"
}
