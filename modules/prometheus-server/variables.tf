variable "name" {
  description = "The name of the environment to deploy to (beta/prod/etc)"
  type        = string
}

variable "key_name" {
  description = "The name of the (AWS) SSH key to associate with the instance"
  type        = string
}

variable "key_file" {
  description = "Path to the SSH private key to provide connection info as output"
  type        = string
}

variable "ami" {
  description = "The base AMI for each AWS instance created"
  type        = string
}

variable "instance_type" {
  description = "The type of AWS instance (size)"
  type        = string
}

variable "root_volume_type" {
  default     = "gp2"
  description = "Type of EBS volume to use for the root block device"
  type        = string
}

variable "root_volume_size" {
  default     = "30"
  description = "Size (in GB) of EBS volume to use for the root block device"
  type        = string
}

variable "data_volume_type" {
  default     = "gp2"
  description = "Type of EBS volume to use for the EBS volume"
  type        = string
}

variable "data_volume_size" {
  default     = "256"
  description = "Size (in GB) of EBS volume to use for the EBS volume"
  type        = string
}

variable "data_volume_encrypted" {
  default     = true
  description = "Boolean, whether or not to encrypt the EBS block device"
  type        = string
}

variable "data_volume_kms_key_id" {
  default     = ""
  description = "ID of the KMS key to use when encyprting the EBS block device"
  type        = string
}

variable "data_volume_snapshot_id" {
  default     = ""
  description = "The ID of the snapshot to base the EBS block device on"
  type        = string
}

variable "data_volume_iops" {
  default     = ""
  description = "The amount of IOPS to provision for the EBS block device"
  type        = string
}

variable "prometheus_pillar" {
  default     = ""
  description = "YAML to pass to prometheus saltstack formula (pillar)"
  type        = string
}

variable "init_prefix" {
  default     = ""
  description = "init shell to run before attaching the EBS volume"
  type        = string
}

variable "init_suffix" {
  default     = ""
  description = "init shell to run after attaching the EBS volume"
  type        = string
}

variable "public_ip" {
  default     = true
  description = "Boolean flag to enable/disable `map_public_ip_on_launch` in the launch configuration"
  type        = string
}

variable "subnet_id" {
  description = "The ID of the subnet to use, depends on the availability zone"
  type        = string
}

variable "security_group_ids" {
  type        = list(string)
  description = "list of security groups (by ID) to associate with the ASG"
}

variable "az" {
  description = "The AWS Availability Zone to deploy to"
  type        = string
}

variable "region" {
  description = "The AWS region to deploy to"
  type        = string
}

variable "load_balancers" {
  default     = []
  description = "The list of load balancers names to pass to the ASG module"
  type        = list(string)
}

