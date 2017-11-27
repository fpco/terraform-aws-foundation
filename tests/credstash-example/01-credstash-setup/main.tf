/*
 * ## Credstash Example 
 *
 * Example demonstrating how to use the `credstash-setup` module.
 *
 */
variable "name" {
  description = "the name of the project"
  default     = "example-project"
}

variable "region" {
  description = "region to deploy to"
  default     = "us-east-2"
}

provider "aws" {
  region = "${var.region}"
}

module "credstash" {
  source               = "../../tf-modules/credstash-setup"
  create_reader_policy = true # can be ommitted if secrets are write-only from within EC2
  create_writer_policy = true # can be ommitted if secrets are read-only from within EC2
  kms_key_name         = "${var.name}-credstash"
  db_table_name        = "${var.name}-credstash"
}

# Below outputs will be used for automatic credstash usage by EC2 instances
output "kms_key_arn" {
  value = "${module.credstash.kms_key_arn}"
}

output "install_snippet" {
  value = "${module.credstash.install_snippet}"
}

output "get_cmd" {
  value = "${module.credstash.get_cmd}"
}

output "put_cmd" {
  value = "${module.credstash.put_cmd}"
}

output "reader_policy_arn" {
  value = "${module.credstash.reader_policy_arn}"
}

output "writer_policy_arn" {
  value = "${module.credstash.writer_policy_arn}"
}

