terraform {
  backend "s3" {
    bucket = "dev-sandbox-tfstate"
    key = "kops-vpc/terraform.tfstate"
    region = "us-east-1"
    encrypt = true
  }
  required_version = ">= 0.11.0"
}

provider "aws" {
  region = "${var.aws_region}"
  version = "1.6.0"
}
