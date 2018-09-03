terraform {
  backend "s3" {
    bucket  = "terraform-remote-state"
    key     = "fpco/mykube-prod/terraform.tfstate-v4"  /* SUPER IMPORTANT TO CHANGE THIS TO A NEW PATH LIKE -v3 or similar */
    region  = "us-east-1"  /* Cannot contain variables */
    encrypt = true
  }
  required_version = ">= 0.11.0"
}

provider "aws" {
  region = "${var.region}"
  version = "1.6.0"
}
