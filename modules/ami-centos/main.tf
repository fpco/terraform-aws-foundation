variable "most_recent" {
  default     = true
  description = "boolean, maps to `most_recent` parameter for `aws_ami` data source"
  type        = bool
}

variable "release" {
  description = "default centos release to target"
  type        = string
}

data "aws_ami" "centos" {
  most_recent = var.most_recent

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  filter {
    name   = "architecture"
    values = ["x86_64"]
  }

  owners = ["679593333241"]

  name_regex = "^CentOS Linux ${var.release} x86_64 HVM EBS ENA"
}

output "id" {
  value       = data.aws_ami.centos.id
  description = "ID of the AMI"
}

