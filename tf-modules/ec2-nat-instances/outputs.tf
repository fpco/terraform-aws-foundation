// List of IDs for the EC2 instance(s)
output "instance_ids" {
  value = ["${module.ec2-nat.instance_ids}"]
}

// List of public IP address(es) for the EC2 instance(s)
output "public_ips" {
  value = ["${module.ec2-nat.public_ips}"]
}

// List of private IP address(es) for the EC2 instance(s)
output "private_ips" {
  value = ["${module.ec2-nat.private_ips}"]
}
