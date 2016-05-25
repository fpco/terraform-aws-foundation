variable "name" {
    description = "The name of the environment to deploy to (beta/prod/etc)"
}
variable "region" {
    description = "The AWS region to deploy to"
}
variable "az" {
    description = "The AWS Availability Zone (AZ) to create the instance in"
}
variable "key_name" {
    description = "The AWS SSH pub key to use in granting access to the instance"
}
variable "key_file" {
    description = "The path to the SSH private key to provide connection info as output"
}
variable "suffix" {
    default = ""
    description = "suffix to the name of the ASG created"
}
variable "initial_init" {
    default = ""
    description = "init shell to run before attaching the EBS volume"
}
variable "extra_init" {
    default = ""
    description = "init shell to run after attaching the EBS volume"
}
variable "instance_type" {
    default = "t2.micro"
    description = "The type of AWS instance (size)"
}
variable "ami" {
    description = "The base AMI for each AWS instance created"
}
variable "subnet_id" {
    description = "The ID of the subnet to use, depends on the availability zone"
}
variable "public_ip" {
    default = "true"
    description = "Boolean flag to enable/disable `map_public_ip_on_launch` in the launch configuration"
}
variable "load_balancers" {
    default = ""
    description = "The string list of names of load balancers to pass to the ASG module"
}
variable "security_group_ids" {
    default = ""
    description = "The string list of security groups ids to pass to the ASG module"
}
variable "root_volume_type" {
    default = "gp2"
    description = "Type of EBS volume to use for the root block device"
}
variable "root_volume_size" {
    default = "15"
    description = "Size (in GB) of EBS volume to use for the root block device"
}
variable "ebs_volume_type" {
    default = "gp2"
    description = "Type of EBS volume to use for the EBS block device"
}
variable "ebs_volume_size" {
    default = "15"
    description = "Size (in GB) of EBS volume to use for the EBS block device"
}
variable "ebs_volume_snapshot_id" {
    default = ""
    description = "The ID of the snapshot to base the EBS block device on"
}
variable "ebs_volume_encrypted" {
    default = "true"
    description = "Boolean, whether or not to encrypt the EBS block device"
}
variable "ebs_volume_kms_key_id" {
    default = ""
    description = "ID of the KMS key to use when encyprting the EBS block device"
}
variable "ebs_volume_iops" {
    default = ""
    description = "The amount of IOPS to provision for the EBS block device"
}
variable "account_arn" {
    description = "The AWS account number, for IAM role assigned to the instance created"
}
variable "access_key" {}
variable "secret_key" {}
