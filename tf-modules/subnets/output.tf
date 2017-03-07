//List of public subnet ids
output "public_ids" {
  value = ["${aws_subnet.public.*.id}"]
}

//List of private subnet ids
output "private_ids" {
  value = ["${aws_subnet.private.*.id}"]
}

