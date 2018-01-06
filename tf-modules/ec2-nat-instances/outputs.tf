// ID of EC2 instance
output "id" {
  value = "${aws_instance.ec2-nat.id}"
}

// Private IP of EC2 instance
output "private_ip" {
  value = "${aws_instance.ec2-nat.private_ip}"
}

// Public IP of EC2 instance
output "public_ip" {
  value = "${aws_instance.ec2-nat.public_ip}"
}

// Public DNS of EC2 instance
output "public_dns" {
  value = "${aws_instance.ec2-nat.public_dns}"
}

// List of private IP address(es) for the EC2 instance(s)
output "private_ips" {
  value = ["${module.ec2-nat.private_ips}"]
}
