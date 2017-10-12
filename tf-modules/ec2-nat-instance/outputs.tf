// ID of EC2 instance
output "id" {
  value = "${aws_instance.ec2-nat.id}"
}

// Public IP of EC2 instance
output "public_ip" {
  value = "${aws_instance.ec2-nat.public_ip}"
}

// Public DNS of EC2 instance
output "public_dns" {
  value = "${aws_instance.ec2-nat.public_dns}"
}
