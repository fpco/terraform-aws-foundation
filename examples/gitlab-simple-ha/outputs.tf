##################
## Outputs

output "region" {
  value       = var.region
  description = "region deployed to"
}

output "gitlab_asg_name" {
  value       = "${var.name}-gitlab-asg-${element(data.aws_availability_zones.available.names, 0)}"
  description = "name of the Gitlab autoscaling group"
}

output "gitlab_url" {
  value       = aws_route53_record.gitlab.name
  description = "URL to gitlab"
}

output "gitlab_server_ip" {
  value       = aws_eip.gitlab.public_ip
  description = "EIP address of gitlab server"
}

output "registry_url" {
  value       = aws_route53_record.registry.name
  description = "URL to docker image registry"
}

// URL to S3 bucket where Docker images are stored
output "registry_bucket_url" {
  value = module.docker-registry-s3-storage.url
}

// Name of the S3 bucket where Docker images are stored
output "registry_bucket_name" {
  value = module.docker-registry-s3-storage.bucket_id
}

