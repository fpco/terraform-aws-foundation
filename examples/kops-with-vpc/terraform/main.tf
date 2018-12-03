provider "aws" {
  region = "${var.region}"
  version = "1.6.0"
}

resource "aws_key_pair" "kops-with-vpc" {
  key_name = "${var.cluster_admin_keyname}"
  public_key = "${file("${var.cluster_admin_keyname}.pem.pub")}"
}
