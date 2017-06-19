variable "name" {
    description = "The name of this auto-scaling cluster, this should be unique"
}
variable "suffix" {
    default = "cluster"
    description = "The suffix to the name of this auto-scaling cluster"
}
variable "key_name" {
    default = ""
    description = "The name of the (AWS) SSH key to associate with the instance"
}
variable "ami" {
    default = ""
    description = "The base AMI for each AWS instance created"
}
variable "iam_profile" {
    default = ""
    description = "The IAM profile to associate with AWS instances in the ASG"
}
variable "instance_type" {
    default = "t2.micro"
    description = "The type of AWS instance (size)"
}
variable "user_data" {
    default = ""
    description = "The user_data string to pass to cloud-init"
}
variable "max_nodes" {
    default = 9
    description = "The maximum number of nodes in each group"
}
variable "min_nodes" {
    default = 3
    description = "The minimum number of nodes in each group"
}
variable "desired_capacity" {
    default = 7
    description = "The desired number of nodes in each group"
}
variable "public_ip" {
    default = "true"
    description = "Boolean flag to enable/disable `map_public_ip_on_launch` in each `aws_subnet`"
}
variable "az_list" {
    default = []
    description = "list of availability zones to associate with the ASG"
}
variable "subnet_ids" {
    default = []
    description = "list of subnets to associate with the ASG (by id)"
}
variable "security_group_ids" {
    default = []
    description = "list of security groups to associate with the ASG (by id)"
}
variable "elb_names" {
    default = []
    description = "list of load balancers to associate with the ASG (by name)"
}
variable "root_volume_type" {
    default = "gp2"
    description = "The type of EBS volume to use for the root block device"
}
variable "root_volume_size" {
    default = "15"
    description = "The size of the EBS volume (in GB) for the root block device"
}
