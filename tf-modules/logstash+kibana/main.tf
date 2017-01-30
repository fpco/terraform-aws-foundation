/**
 *## Logstash and Kibana
 *
 * This module joins together `../logstash` and `../kibana` modules, thus instead of
 * deplying EC2 instance per service, it deploys a server that runs both services.
 *
 */

module "kibana" {
  source = "../kibana"

  name_prefix          = "${var.name_prefix}"
  vpc_id               = "${var.vpc_id}"
  vpc_azs              = ["${var.vpc_azs}"]
  route53_zone_id      = "${var.route53_zone_id}"
  kibana_dns_name      = "${var.kibana_dns_name}"
  subnet_ids           = ["${var.subnet_ids}"]
  key_name             = ""
  ami                  = ""
  instance_type        = ""
  elasticsearch_url    = "${var.elasticsearch_url}"
  min_server_count     = 0
  max_server_count     = 0
  desired_server_count = 0
}

module "logstash-kibana" {
  source = "../logstash"

  name_prefix           = "${var.name_prefix}-kibana"
  vpc_id                = "${var.vpc_id}"
  vpc_azs               = ["${var.vpc_azs}"]
  route53_zone_id       = "${var.route53_zone_id}"
  logstash_dns_name     = "${var.logstash_dns_name}"
  subnet_ids            = ["${var.subnet_ids}"]
  ami                   = "${var.ami}"
  instance_type         = "${var.instance_type}"
  key_name              = "${var.key_name}"
  elasticsearch_url     = "${var.elasticsearch_url}"
  min_server_count      = "${var.min_server_count}"
  max_server_count      = "${var.max_server_count}"
  desired_server_count  = "${var.desired_server_count}"
  extra_security_groups = ["${module.kibana.security_group_id}"]
  extra_setup_snippet   = "${module.kibana.setup_snippet}"
  extra_elbs            = ["${module.kibana.elb_name}"]
  ca_cert               = "${var.logstash_ca_cert}"
  server_cert           = "${var.logstash_server_cert}"
  credstash_kms_key_arn = "${var.credstash_kms_key_arn}"
  credstash_server_key_name = "${var.credstash_server_key_name}"
  credstash_dynamic_config_name = "${var.credstash_dynamic_config_name}"

}
