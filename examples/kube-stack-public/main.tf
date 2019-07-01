provider "aws" {
  region = var.region
}

resource "aws_key_pair" "main" {
  key_name   = var.name
  public_key = file(var.ssh_pubkey)
}

