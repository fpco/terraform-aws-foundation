variable "name" {
  description = "name of the project, use as prefix to names of resources created"
  default = "gitlab-asg-test"
}

variable "region" {
  description = "AWS Region where the project will be deployed"
  default = "us-east-1"
}

variable "ssh_pubkey" {
  description = "The path to the SSH public key"
  default     = "./id_rsa.pub"
}

variable "dns_zone_name" {
  description = "The name of the DNS zone on Route53 (example.com), to create records in for gitlab"
  type        = "string"
}

variable "gitlab_name" {
  description = "To generate the DNS record for gitlab, prefix the zone"
  default     = "gitlab"
  type        = "string"
}

variable "gitlab_registry_name" {
  description = "To generate the DNS record for the docker registry, prefix the zone"
  default     = "registry"
  type        = "string"
}

variable "root_volume_size" {
  default     = "30"
  description = "GB of root data volume for the instance, make it larger than usual for docker builds"
}

variable "registry_bucket_name" {
  description = "The name of the S3 bucket to write docker images to"
  type        = "string"
}

variable "gitlab_backup_bucket_name" {
  description = "The name of the S3 bucket to write the backups"
  type        = "string"
}

variable "gitlab_data_volume_size" {
  description = "GB of EBS data volume for the instance to mount gitlab"
  default     = "20"
}

variable "gitlab_s3_aws_access_key" {
  description = "AWS access key to get access to buckup s3"
  default     = ""
}

variable "gitlab_s3_aws_secret_key" {
  description = "AWS secret key to get access to buckup s3"
  default     = ""
}

variable "config_elb" {
  default     = false
  description = "variable to determine how to set up gitlab configuration. Setting to false uses the EIP version of module tuned to ELB"
}
