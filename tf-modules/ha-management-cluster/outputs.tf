# module outputs!
output "services_security_group" {
    value = "${aws_security_group.management_services.id}"
}
output "nomad_server_sg" {
    value = "${module.nomad-server-sg.id}"
}
output "nomad_agent_sg" {
    value = "${module.nomad-agent-sg.id}"
}
output "asg_name" {
    value = "${module.management-cluster.asg_name}"
}
