output "public_ip" {
  value       = module.asg.eip_address
  description = "Public IP of the tunnel"
}

output "sg_id" {
  value       = module.tunnel-sg.id
  description = "Security group id of the tunnel"
}

output "asg_name" {
  value       = module.asg.asg_name
  description = "name of the tunnel ASG"
}

output "asg_iam_role_name" {
  value       = module.asg.asg_iam_role_name
  description = "name of ASG IAM role"
}

output "data_volume_name_tag" {
  value       = module.asg.data_volume_name_tag
  description = "Name tag value for attached data volume."
}
