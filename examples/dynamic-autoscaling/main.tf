/**
 * ## Dynamic AWS Autoscaling of EC2 instances
 *
 * Creates an auto-scaling group that responds to metrics
 * for increasing and decreasing instance counts
 *
 */

terraform {
  backend "local" {
    path = "./terraform.tfstate"
  }
}

provider "aws" {
  version = "~> 1.10"
  region  = "${var.region}"
}
