//`id` exported from `aws_security_group`
output "id" {
  value = "${aws_security_group.main.id}"
}

//`name` exported from `aws_security_group`
output "name" {
  value = "${aws_security_group.main.name}"
}

//`ingress` exported from `aws_security_group`
output "ingress" {
  value = "${aws_security_group.main.ingress}"
}
