# ------------------------------------------------------------------------------
# Variables
# ------------------------------------------------------------------------------
variable "name_prefix" {
  description = "Prefix used for resource names."
}

variable "azs" {
  description = "list of availability zones to associate with the ASG"
  type        = "list"
}

variable "elb_names" {
  default     = []
  description = "list of load balancers to associate with the ASG (by name)"
  type        = "list"
}

variable "subnet_ids" {
  description = "IDs of subnets where the instances will be provisioned."
  type        = "list"
}

variable "instance_count" {
  description = "Desired (and minimum) number of instances."
  default     = "2"
}

variable "instance_ami" {
  description = "ID of an Amazon Linux 2 AMI. (Comes with SSM agent installed)"
  default     = "ami-db51c2a2"
}

variable "instance_type" {
  description = "Type of instance to provision."
  default     = "t2.micro"
}

variable "instance_key" {
  description = "Name of an EC2 key pair which will be allowed to SSH to the instance."
  default     = ""
}

variable "elb_sg_id" {
  description = "ID of the VPC for the subnets."
}

variable "asg_template_file" {
  description = "Cloud init script for the autoscaling group"
}

variable "sns_topic_arn" {
  description = "ARN for the SNS topic"
}

variable "vpc_id" {
  description = "ID of the VPC for the subnets."
}

variable "elb_arn" {
  description = "ARN for the elb"
}

variable "aws_role_arn" {
  description = "ARN for the AWS Role"
}

variable "aws_instance_ec2_name" {
  description = "AWS IAM EC2 Name"
}

variable "aws_sg_id" {
  description = "AWS Security group ID"
}
