variable "vpc_id" {
  type        = string
  description = "ID of the VPC."
}

variable "identifier" {
  type        = string
  description = "Identifier of related resources."
}

variable "region" {
  type        = string
  description = "AWS region for this bastion to be in."
}

variable "key_name" {
  type        = string
  description = "SSH key pair name for the bastion."
}

variable "public_subnet_id" {
  type        = string
  description = "The subnet for the bastion. The subnet must be able to access Internet."
}

variable "instance_type" {
  type        = string
  default     = "t2.nano"
  description = "Bastion instance type."
}

variable "egress_cidrs" {
  type        = list(string)
	default     = ["0.0.0.0/0"]
  description = "Egress subnets that bastion can access."
}

module "instance" {
  source             = "../single-node-asg"
  name_prefix        = var.identifier
  name_suffix        = "bastion"
  ami                = module.ubuntu-ami.id
  instance_type      = var.instance_type
  region             = var.region
  key_name           = var.key_name
  subnet_id          = var.public_subnet_id
  security_group_ids = [aws_security_group.bastion.id]
  assign_eip         = true
}

resource "aws_security_group" "bastion" {
  name   = "${var.identifier}-bastion"
  vpc_id = var.vpc_id

	ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port       = 0
    to_port         = 0
    protocol        = "-1"
    cidr_blocks     = var.egress_cidrs
  }
}

module "ubuntu-ami" {
  source  = "../../modules/ami-ubuntu"
  release = "18.04"
}
