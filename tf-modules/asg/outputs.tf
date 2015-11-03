# name of the ASG
output "name" {
    value = "${aws_autoscaling_group.cluster.name}"
}
# ID of the ASG
output "id" {
    value = "${aws_autoscaling_group.cluster.id}"
}
output "lc_id" {
    value = "${aws_launch_configuration.cluster.id}"
}
# name of the Launch Config
output "lc_name" {
    value = "${aws_launch_configuration.cluster.name}"
}
