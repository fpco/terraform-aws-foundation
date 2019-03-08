output "url" {
  value = "http://${aws_instance.web.public_ip}"
}
