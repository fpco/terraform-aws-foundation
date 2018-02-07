variable "name" {
  description = "The name of the environment to deploy to (beta/prod/etc)"
}

variable "key_name" {
  description = "The name of the (AWS) SSH key to associate with the instance"
}

variable "key_file" {
  description = "Path to the SSH private key to provide connection info as output"
}

variable "ami" {
  description = "The base AMI for each AWS instance created"
}

variable "instance_type" {
  description = "The type of AWS instance (size)"
}

variable "root_volume_type" {
  default     = "gp2"
  description = "Type of EBS volume to use for the root block device"
}

variable "root_volume_size" {
  default     = "30"
  description = "Size (in GB) of EBS volume to use for the root block device"
}

variable "data_volume_type" {
  default     = "gp2"
  description = "Type of EBS volume to use for the EBS volume"
}

variable "data_volume_size" {
  default     = "256"
  description = "Size (in GB) of EBS volume to use for the EBS volume"
}

variable "data_volume_encrypted" {
  default     = "true"
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

variable "data_volume_iops" {
  default     = ""
  description = "The amount of IOPS to provision for the EBS block device"
}

variable "prometheus_pillar" {
  default     = ""
  description = "YAML to pass to prometheus saltstack formula (pillar)"
}

variable "init_prefix" {
  default     = ""
  description = "init shell to run before attaching the EBS volume"
}

variable "init_suffix" {
  default     = ""
  description = "init shell to run after attaching the EBS volume"
}

variable "public_ip" {
  default     = "true"
  description = "Boolean flag to enable/disable `map_public_ip_on_launch` in the launch configuration"
}

variable "subnet_id" {
  description = "The ID of the subnet to use, depends on the availability zone"
}

variable "security_group_ids" {
  type        = "list"
  description = "list of security groups (by ID) to associate with the ASG"
}

variable "region" {
  description = "The AWS region to deploy to"
}

variable "load_balancers" {
  default     = []
  description = "The list of load balancers names to pass to the ASG module"
}

variable "account_arn" {
  description = "The AWS account number, for IAM role assigned to the ASG"
}
