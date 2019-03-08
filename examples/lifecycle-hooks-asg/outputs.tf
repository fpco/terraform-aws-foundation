output "instance_ids" {
    value = ["${aws_instance.web.*.public_ip}"]
}

output "elb_dns_name" {
  value = "${aws_elb.example.dns_name}"
}
