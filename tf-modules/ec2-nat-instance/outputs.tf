output "dns" {
  value = "${aws_instance.ec2-nat.public_dns}"
}
