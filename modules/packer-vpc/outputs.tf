output "vpc_id" {
  value       = aws_vpc.packer.id
  description = "VPC ID"
}

output "subnet_id" {
  value       = aws_subnet.packer.id
  description = "Subnet ID"
}

output "trusty_ami_id" {
  value       = data.aws_ami.ubuntu-trusty.id
  description = "ID of latest trusty AMI"
}

output "xenial_ami_id" {
  value       = data.aws_ami.ubuntu-xenial.id
  description = "ID of latest xenial AMI"
}

