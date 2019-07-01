variable "name_prefix" {
  default     = "dev"
  description = "Prefix that will be added to names of all resources"
  type        = string
}

variable "name_suffix" {
  default     = ""
  description = "Suffix to use for Scaling Group, EC2 instances."
  type        = string
}

variable "ami" {
  description = "AMI to use for instances running Logstash"
  type        = string
}

variable "instance_type" {
  default = "t2.micro"
  type    = string
}

variable "min_server_count" {
  description = "Minimum number of EC2 instances running Logstash"
  default     = 1
  type        = string
}

variable "max_server_count" {
  description = "Maximum number of EC2 instances running Logstash"
  default     = 1
  type        = string
}

variable "desired_server_count" {
  description = "Desired number of EC2 instances running Logstash"
  default     = 1
  type        = string
}

variable "vpc_id" {
  description = "VPC id where Logstash servers should be deployed in"
  type        = string
}

variable "private_subnet_ids" {
  description = "A list of private subnet ids to deploy Logstash servers in"
  type        = list(string)
}

variable "public_subnet_ids" {
  description = "A list of public subnet ids to deploy Logstash ELB in"
  type        = list(string)
}

variable "logstash_dns_name" {
  description = "DNS name for Logstash endpoint"
  type        = string
}

variable "elasticsearch_url" {
  description = "Elasticsearch endpoint url"
  type        = string
}

variable "key_name" {
  description = "SSH key name to use for connecting to all nodes"
  type        = string
}

variable "certstrap_depot_path" {
  default     = ""
  description = "Local path, where generated SSL certifcates will be stored in. Certificates will be removed from local file system if left empty and will only be retained in credential store"
  type        = string
}

variable "certstrap_ca_force_new" {
  default     = false
  description = "New CA certificate will be created even if there already one exitsts with the same CN"
  type        = string
}

variable "certstrap_ca_common_name" {
  default     = "Logstash"
  description = "Common Name to be used during CA certificate generation"
  type        = string
}

variable "certstrap_ca_passphrase" {
  default     = ""
  description = "Passphrase for SSL Key encryption to be used during CA certificate generation"
  type        = string
}

variable "credstash_kms_key_arn" {
  description = "Master KMS key ARN for getting SSL server key using credstash"
  type        = string
}

variable "credstash_reader_policy_arn" {
  description = "Secrets Reader Policy ARN that was created by 'credstash-setup' module. Reading will be disabled if not supplied."
  type        = string
}

variable "credstash_install_snippet" {
  description = "Ubuntu bash script snippet for installing credstash and its dependencies"
  type        = string
}

variable "credstash_get_cmd" {
  description = "Credstash get command with region and table values set."
  type        = string
}

variable "credstash_put_cmd" {
  description = "Credstash put command with region, table and kms key values set."
  type        = string
}

variable "extra_setup_snippet" {
  default     = ""
  description = "Extra snippet to run after logstash has been installed and configured"
  type        = string
}

variable "extra_sg_ids" {
  default     = []
  description = "Extra Security Group IDs that will be added to all instances running Logstash. This is a way to add extra services, SSH access for instance."
  type        = list(string)
}

variable "extra_elb_ingress_cidrs" {
  default     = []
  description = "Extra CIDRs that are allowed to access Logstash. By default only CIDR from `public_subnet_ids` are allowed"
  type        = list(string)
}

variable "extra_settings" {
  default     = ""
  description = "Extra Logstash setting in YAML format"
  type        = string
}

variable "extra_config" {
  default     = ""
  description = "Extra Logstash configuration. It will in the middle of the pipeline, between the main config, and the one that can be supplied through credstash with 'credstash_dynamic_config_name' keyname."
  type        = string
}

variable "extra_grok_patterns" {
  default     = ""
  description = "Extra grok Patterns for Logstash, which can be used during log parsing by setting: patterns_dir => ['/etc/logstash/patterns']"
  type        = string
}

variable "target_group_arns" {
  default     = []
  description = "Application Load Balancer target groups to be used for Logstash auto scaling group"
  type        = list(string)
}

variable "elb_names" {
  default     = []
  description = "Extra ELBs that should be attached to ASG. Useful for attaching Kibana ELB."
  type        = list(string)
}

variable "internal" {
  default     = true
  description = "Set it to false if you want Logstash to be accessible by the outside world"
  type        = string
}

variable "logstash_version" {
  description = "Which version of Logstash to install"
  type        = string
}

