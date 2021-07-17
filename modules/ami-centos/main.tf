variable "most_recent" {
  default     = true
  description = "boolean, maps to `most_recent` parameter for `aws_ami` data source"
  type        = bool
}

variable "release" {
  description = "default centos release to target"
  type        = number
  validation {
    condition     = var.release >= 6 && var.release <= 8
    error_message = "CentOS version number (6 | 7 | 8)."
  }
}

variable "image_provider" {
  description = "The image provider (AWS | CentOS)."
  type        = string
  validation {
    condition     = var.image_provider == "CentOS" || var.image_provider == "AWS"
    error_message = "The provider value must be either \"CentOS\" or \"AWS\"."
  }
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
    name = "product-code"
    values = compact(var.image_provider == "AWS" ? [
      var.release == 8 ? "47k9ia2igxpcce2bzo8u3kj03" : "", # Official `CentOS 8 (x86_64) - with Updates HVM` by AWS product id
      var.release == 7 ? "cvugziknvmxgqna9noibqnnsy" : "", # Official `CentOS 7 (x86_64) - with Updates HVM` by AWS product id
      var.release == 6 ? "ckx0h8ljio731afm2k92jtg62" : ""  # Official `CentOS 6 (x86_64) - with Updates HVM` by AWS product id
      ] : [
      var.release == 7 ? "aw0evgkw8e5c1q413zgy5pjce" : "", # Official `CentOS 7 (x86_64) - with Updates HVM` by CentOS product id
      var.release == 6 ? "6x5jmcajty9edm3f211pqjfn2" : ""  # Official `CentOS 6 (x86_64) - with Updates HVM` by CentOS product id
    ])
  }

  owners = ["679593333241"]

}

output "id" {
  value       = data.aws_ami.centos.id
  description = "ID of the AMI"
}

output "name" {
  value       = data.aws_ami.centos.name
  description = "Name of the AMI"
}

