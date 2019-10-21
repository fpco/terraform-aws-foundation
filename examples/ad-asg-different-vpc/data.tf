provider "aws" {
  # Requester's credentials
}

provider "aws" {
  alias  = "peer"
  # Accepter's credentials.
}

data "aws_caller_identity" "requester" {
}

data "aws_caller_identity" "peer" {
  provider = "aws.peer"
}

data "aws_availability_zones" "available" {
}

data "aws_region" "current" {}
