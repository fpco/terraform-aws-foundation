# ------------------------------------------------------------------------------
# Variables
# ------------------------------------------------------------------------------
variable "name_prefix" {
  description = "Prefix used for resource names."
}

variable "azs" {
  description = "list of availability zones to associate with the ASG"
  type        = list(string)
}

variable "elb_names" {
  default     = []
  description = "list of load balancers to associate with the ASG (by name)"
  type        = list(string)
}

variable "subnet_ids" {
  description = "IDs of subnets where the instances will be provisioned."
  type        = list(string)
}

variable "instance_count" {
  description = "Desired (and minimum) number of instances."
  default     = "2"
  type = number
}

variable "instance_ami" {
  description = "ID of an Amazon Linux 2 AMI. (Comes with SSM agent installed)"
  default     = "ami-db51c2a2"
  type = string
}

variable "instance_type" {
  description = "Type of instance to provision."
  default     = "t2.micro"
  type = string
}

variable "instance_key" {
  description = "Name of an EC2 key pair which will be allowed to SSH to the instance."
  default     = ""
  type = string
}

variable "elb_sg_id" {
  description = "ID of the VPC for the subnets."
  type = string
}

variable "asg_template_file" {
  description = "Cloud init script for the autoscaling group"
  type = string
}

variable "sns_topic_arn" {
  description = "ARN for the SNS topic"
  type = string
}

variable "vpc_id" {
  description = "ID of the VPC for the subnets."
  type = string
}

variable "elb_arn" {
  description = "ARN for the elb"
  type = string
}

variable "aws_role_arn" {
  description = "ARN for the AWS Role"
  type = string
}

variable "aws_instance_ec2_name" {
  description = "AWS IAM EC2 Name"
  type = string
}

variable "aws_sg_id" {
  description = "AWS Security group ID"
  type = string
}
