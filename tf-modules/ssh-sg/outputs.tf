//`id` exported from `aws_security_group`
output "id" {
  value = "${aws_security_group.ssh.id}"
}

//`name` exported from `aws_security_group`
output "name" {
  value = "${aws_security_group.ssh.name}"
}
