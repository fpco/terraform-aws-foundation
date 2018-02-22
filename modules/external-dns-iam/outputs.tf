output "dnscontroller_iam_role" {
  value       = "${aws_iam_role.dnscontroller.name}"
  description = "Name of the new role with route53 permissions."
}

output "dnscontroller_iam_role_arn" {
  value       = "${aws_iam_role.dnscontroller.arn}"
  description = "ARN of the new role with route53 permissions."
}

