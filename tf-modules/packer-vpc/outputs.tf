// VPC ID
output "vpc_id" {
  value = "${aws_vpc.packer.id}"
}

// Subnet ID
output "subnet_id" {
  value = "${aws_subnet.packer.id}"
}

// ID of latest trusty AMI
output "trusty_ami_id" {
  value = "${data.aws_ami.ubuntu-trusty.id}"
}

// ID of latest xenial AMI
output "xenial_ami_id" {
  value = "${data.aws_ami.ubuntu-xenial.id}"
}
