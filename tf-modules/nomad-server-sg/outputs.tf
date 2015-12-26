output "id" {
    value = "${aws_security_group.main.id}"
}
output "name" {
    value = "${aws_security_group.main.name}"
}
output "ingress" {
    value = "${aws_security_group.main.ingress}"
}
