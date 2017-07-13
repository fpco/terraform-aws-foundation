// AMI that was used for all EC2 instances in the stack
output "ami" {
  value = "${data.aws_ami.ubuntu.id}"
}

// Security group that allows SSH access
output "ssh_sg_id" {
  value = "${aws_security_group.ssh.id}"
}
