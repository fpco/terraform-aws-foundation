variable "name" {
  description = "The name of the account or deployment to use with this policy."
  default = "example"
}

variable "region" {
  description = "Region where the project will be deployed"
  default     = "us-west-2"
}

variable "bucket_names" {
  description = "list of bucket names to grant access to"
  default = ["s3-full-access-policy-bucket"]
}

provider "aws" {
  region = "${var.region}"
}

module "s3-full-access-policy" {
  source       = "../../modules/s3-full-access-policy"
  name         = "${var.name}"
  bucket_names = "${var.bucket_names}"
}

resource "aws_s3_bucket" "test-bucket" {
  bucket = "${var.bucket_names[0]}"
  acl = "private"

  tags {
    Name = "Test bucket for s3-full-access-policy module"
  }
}
