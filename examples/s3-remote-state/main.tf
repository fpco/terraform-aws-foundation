variable "region" {
  description = "The AWS region to deploy to."
  type        = string
}

variable "wordpress_ami_id" {
  default     = "ami-092e8c73"
  description = "The AMI to use as the base."
}

variable "remote_state_bucket_name" {
  description = "Bucket name on AWS S3 to store remote state in."
}

variable "remote_state_principals" {
  default     = []
  description = "list of user/role ARNs to get full access to the bucket"
}

variable "remote_state_versioning" {
  default     = "true"
  description = "enables versioning for objects in the S3 bucket"
}

variable "remote_state_region" {
  default     = ""
  description = "The AWS region to store the remote state in."
}

variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}

provider "aws" {
  region = var.region
}

module "s3-remote-state" {
  source      = "../../modules/s3-remote-state"
  region      = var.remote_state_region
  bucket_name = var.remote_state_bucket_name
}

resource "aws_security_group" "wordpress" {
  name = "example-s3-remote-state-wordpress"
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  ingress {
    from_port   = 8080
    to_port     = 8080
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_instance" "wordpress" {
  ami                         = var.wordpress_ami_id
  associate_public_ip_address = true
  instance_type               = "t2.micro"
  security_groups             = [aws_security_group.wordpress.name]
  tags = {
    Name = "wordpress"
  }
}

output "region" {
  value       = var.region
  description = "AWS region"
}

output "wordpress_public_ip" {
  value       = aws_instance.wordpress.public_ip
  description = "Public IP of the created wordpress endpoint"
}

output "remote_state_principals" {
  value = module.s3-remote-state.principals
}

