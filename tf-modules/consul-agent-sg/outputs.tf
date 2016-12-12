//Security group ID
output "id" {
    value = "${aws_security_group.main.id}"
}
//Security group name
output "name" {
    value = "${aws_security_group.main.name}"
}
//Security group ingress rules
output "ingress" {
    value = "${aws_security_group.main.ingress}"
}
