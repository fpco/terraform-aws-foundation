variable "name_prefix" {
  default = "dev"
  description = "Prefix that will be added to names of all resources"
}

variable "name_suffix" {
  default = ""
  description = "Suffix to use for Scaling Group, EC2 instances."
}

variable "ami" {
  description = "AMI to use for instances running Logstash"
}

variable "instance_type" {
  default = "t2.micro"
}

variable "min_server_count" {
  description = "Minimum number of EC2 instances running Logstash"
  default = 1
}

variable "max_server_count" {
  description = "Maximum number of EC2 instances running Logstash"
  default = 1
}

variable "desired_server_count" {
  description = "Desired number of EC2 instances running Logstash"
  default = 1
}

variable "vpc_id" {
  description = "VPC id where Logstash servers should be deployed in"
}

variable "private_subnet_ids" {
  description = "A list of private subnet ids to deploy Logstash servers in"
  type = "list"
}

variable "public_subnet_ids" {
  description = "A list of public subnet ids to deploy Logstash ELB in"
  type = "list"
}

variable "logstash_dns_name" {
  description = "DNS name for Logstash endpoint"
}

variable "route53_zone_id" {
  description = "Route53 Zone id where ELB should get added a record to"
}

variable "elasticsearch_url" {
  description = "Elasticsearch endpoint url"
}

variable "key_name" {
  description = "SSH key name to use for connecting to all nodes"
}

variable "certstrap_depot_path" {
  default = ""
  description = "Local path, where generated SSL certifcates will be stored in. Certificates will be removed from local file system if left empty and will only be retained in credential store"
}

variable "certstrap_ca_common_name" {
  default = "Logstash"
  description = "Common Name to be used during CA certificate generation"
}

variable "certstrap_ca_passphrase" {
  default = ""
  description = "Passphrase for SSL Key encryption to be used during CA certificate generation"
}

variable "credstash_table_name" {
  default = "credential-store"
  description = "DynamoDB table used by credstash to store credentials"
}

variable "credstash_kms_key_arn" {
  description = "Master KMS key ARN for getting SSL server key using credstash"
}

variable "credstash_prefix" {
  default = "dev-"
  description = "Prefix to be used for all names used by credstash"
}
variable "credstash_ca_cert_name" {
  default = "logstash-ca-cert"
  description = "CA certificate will be genrated and stored using credstash under this name. Logstash is configured to trust clients with certificates signed by this CA"
}

variable "credstash_ca_key_name" {
  default = "logstash-ca-key"
  description = "CA SSL Key that corresponds to CA certificate will be stored under this name"
}

variable "credstash_server_key_name" {
  default = "logstash-ssl-key"
  description = "Name of the SSL server key, which will be used by credstash to identify it"
}

variable "credstash_server_cert_name" {
  default = "logstash-ssl-cert"
  description = "Name of the SSL server certificate, which will be used by credstash to identify it"
}

variable "credstash_dynamic_config_name" {
  default = "logstash-dynamic-conf"
  description = "This a key for credstash to be used to poll dynamic configuration for logstash, thus creating an ability to remotely update logstash fiters during runtime."
}

variable "extra_setup_snippet" {
  default = ""
  description = "Extra snippet to run after logstash has been installed and configured"
}

variable "extra_sg_ids" {
  default = []
  description = "Extra Security Group IDs that will be added to all instances running Logstash. This is a way to add extra services, SSH access for instance."
}

variable "extra_elb_sg_ids" {
  default = []
  description = "Extra Security Group IDs that will be added to Logstash Load Balancer"
}

variable "extra_elb_ingress_cidrs" {
  default = []
  description = "Extra CIDRs that are allowed to access Logstash. By default only CIDR from `public_subnet_ids` are allowed"
}

variable "extra_settings" {
  default = ""
  description = "Extra Logstash setting in YAML format"
}

variable "extra_elbs" {
  default = []
  description = "Elastic Load Balancers, besides the default one, to be used for Logstash scaling group"
}

variable "internal" {
  default = true
  description = "Set it to false if you want Logstash to be accessible by the outside world"
}
