//`name` exported from the Server `aws_autoscaling_group`
output "asg_name" {
    value = "${module.server.name}"
}

//`arn` exported from the Service Data `aws_iam_role`
output "asg_iam_role_arn" {
  value = "${module.service-data.iam_role_arn}"
}
