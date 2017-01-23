/**
 *## Logstash and Kibana
 *
 * This module joins together `../logstash` and `../kibana` modules, thus instead of
 * deplying an instance per service, it deploys a server that runs both services.
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

module "logstash" {
  source = "../logstash"

  name_prefix          = "${var.name_prefix}"
  vpc_id               = "${var.vpc_id}"
  vpc_azs              = ["${var.vpc_azs}"]
  route53_zone_id      = "${var.route53_zone_id}"
  logstash_dns_name    = "${var.logstash_dns_name}"
  subnet_ids           = ["${var.subnet_ids}"]
  key_name             = ""
  ami                  = ""
  instance_type        = ""
  elasticsearch_url    = "${var.elasticsearch_url}"
  min_server_count     = 0
  max_server_count     = 0
  desired_server_count = 0
  ca_cert              = "${var.logstash_ca_cert}"
  server_cert          = "${var.logstash_server_cert}"
  server_key           = "${var.logstash_server_key}"
}


resource "aws_autoscaling_group" "logstash-kibana-asg" {
  availability_zones   = ["${var.vpc_azs}"]
  vpc_zone_identifier  = ["${var.subnet_ids}"]
  name                 = "${var.name_prefix}-logstash-kibana-asg"
  max_size             = "${var.max_server_count}"
  min_size             = "${var.min_server_count}"
  desired_capacity     = "${var.desired_server_count}"
  launch_configuration = "${aws_launch_configuration.logstash-kibana-lc.name}"
  health_check_type    = "ELB"
  load_balancers       = ["${module.logstash.elb_name}", "${module.kibana.elb_name}"]

  tag = [{
    key                 = "Name"
    value               = "${var.name_prefix}-logstash-kibana"
    propagate_at_launch = true
  }]

}


resource "aws_launch_configuration" "logstash-kibana-lc" {

  name_prefix          = "${var.name_prefix}-logstash-kibana-lc-"
  image_id             = "${var.ami}"
  instance_type        = "${var.instance_type}"
  key_name             = "${var.key_name}"
  security_groups      = ["${module.logstash.security_group_id}", "${module.kibana.security_group_id}"]
  user_data            = <<USER_DATA
#!/bin/bash
${module.logstash.setup_snippet}
${module.kibana.setup_snippet}
USER_DATA

  associate_public_ip_address = true

  lifecycle = {
    create_before_destroy = true
  }
  
}
