provider "aws" {
  region = var.region
}

resource "aws_key_pair" "kops-with-vpc" {
  key_name   = var.cluster_admin_keyname
  public_key = file("${var.cluster_admin_keyname}.pem.pub")
}

