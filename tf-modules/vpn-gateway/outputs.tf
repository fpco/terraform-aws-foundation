output "ami" {
  value = "${data.aws_ami.ubuntu.id}"
}
output "public_ip" {
  value = "${aws_instance.vpn-gateway.public_ip}"
}
