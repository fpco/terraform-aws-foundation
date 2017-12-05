variable "name" {
  description = "The name of the environment to deploy to (beta/prod/etc)"
  type        = "string"
}

variable "key_name" {
  description = "The name of the (AWS) SSH key to associate with the instance"
  type        = "string"
}

variable "key_file" {
  description = "Path to the SSH private key to provide connection info as output"
  type        = "string"
}

variable "ami" {
  description = "The base AMI for each AWS instance created"
  type        = "string"
}

variable "instance_type" {
  default     = "t2.micro"
  description = "The type of AWS instance (size)"
}

variable "data_volume_size" {
  default     = "2"
  description = "Size (in GB) of EBS volume to use for the EBS volume"
}

variable "data_volume_encrypted" {
  default     = "false"
  description = "Boolean, whether or not to encrypt the EBS block device"
}

variable "data_volume_kms_key_id" {
  default     = ""
  description = "ID of the KMS key to use when encyprting the EBS block device"
}

variable "data_volume_snapshot_id" {
  default     = ""
  description = "The ID of the snapshot to base the EBS block device on"
}

variable "init_suffix" {
  default     = ""
  description = "init shell to run after setting VOLUME_ID and REGION exports"
}

variable "public_ip" {
  default     = "false"
  description = "Boolean flag to enable/disable `map_public_ip_on_launch` in the launch configuration"
}

variable "subnet_id" {
  description = "The ID of the subnet to use, depends on the availability zone"
  type        = "string"
}

variable "security_group_ids" {
  description = "The list of security groups (by ID) to associate with the ASG"
  type        = "list"
}

variable "region" {
  description = "The AWS region to deploy to"
  type        = "string"
}

variable "az" {
  description = "The AWS Availability Zone (AZ) to create the instance in"
  type        = "string"
}

variable "load_balancers" {
  default     = []
  description = "The list of load balancers names to pass to the ASG module"
}

variable "aws_cloud" {
  description = "set to 'aws-us-gov' if using GovCloud, otherwise leave the default"
  default     = "aws"
}
