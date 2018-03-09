output "name" {
  value       = "${aws_iam_role.cloudwatch_exporter.name}"
  description = "Name of the new role with cloudwatch read-only permissions."
}

output "arn" {
  value       = "${aws_iam_role.cloudwatch_exporter.arn}"
  description = "ARN of the new role with cloudwatch read-only permissions."
}

