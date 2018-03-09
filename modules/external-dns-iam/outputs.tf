output "name" {
  value       = "${aws_iam_role.dnscontroller.name}"
  description = "Name of the new role with route53 permissions."
}

output "arn" {
  value       = "${aws_iam_role.dnscontroller.arn}"
  description = "ARN of the new role with route53 permissions."
}

