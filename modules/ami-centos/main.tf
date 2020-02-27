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

  # this filter is here to guarantee that ami's come from the official CentOS.org
  # we can be sure of the ami's authenticity by filtering by the products id's unique to
  # CentOS.org, which can be found on their web site at https://wiki.centos.org/Cloud/AWS
  filter {
    name   = "product-code"
    values = [
      "aw0evgkw8e5c1q413zgy5pjce", # Official `CentOS 7 (x86_64) - with Updates HVM` product id
      "6x5jmcajty9edm3f211pqjfn2"  # Official `CentOS 6 (x86_64) - with Updates HVM` product id
    ]
  }

  owners = ["679593333241"]

  name_regex = "^CentOS Linux ${var.release} x86_64 HVM EBS ENA"
}

output "id" {
  value       = data.aws_ami.centos.id
  description = "ID of the AMI"
}

