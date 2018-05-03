variable "name" {
  description = "The name of the account or deployment to use with this policy."
  default = "s3-full-access-policy-example"
}

variable "region" {
  description = "Region where the project will be deployed"
  default     = "us-west-2"
}

variable "bucket_names" {
  description = "list of bucket names to grant access to"
  default = []
}

provider "aws" {
  region = "${var.region}"
}

module "s3-full-access-policy" {
  source       = "../../modules/s3-full-access-policy"
  bucket_names = "${var.bucket_names}"
}
