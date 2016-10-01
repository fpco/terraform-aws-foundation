# these variables should be either specified on the command line
# or kept in `auth.tfvars` which is never to be checked in
variable "aws_access_key" {}
variable "aws_secret_key" {}

variable "aws_region" {
  description = "default region in which to operate"
  default = "us-west-2"
}
