/**
 * ## Consul Demo Server
 *
 * Run an instance of consul in server mode, on a single-node auto-scaling group.
 * Use an EBS volume for data persistence.
 *
 * **TODO: Add an example that shows how to use this module..**
 *
 */

module "consul-server" {
  source                  = "../single-node-asg"
  name                    = "${var.name}"
  name_suffix             = "consul-server"
  aws_cloud               = "${var.aws_cloud}"
  region                  = "${var.region}"
  az                      = "${var.az}"
  root_volume_type        = "gp2"
  root_volume_size        = "8"
  data_volume_type        = "gp2"
  data_volume_size        = "${var.data_volume_size}"
  data_volume_encrypted   = "${var.data_volume_encrypted}"
  data_volume_kms_key_id  = "${var.data_volume_kms_key_id}"
  data_volume_snapshot_id = "${var.data_volume_snapshot_id}"
  key_name                = "${var.key_name}"
  key_file                = "${var.key_file}"
  ami                     = "${var.ami}"
  instance_type           = "${var.instance_type}"
  public_ip               = "${var.public_ip}"
  subnet_id               = "${var.subnet_id}"
  load_balancers          = ["${var.load_balancers}"]
  security_group_ids      = ["${var.security_group_ids}"]

  init_suffix = <<END_INIT
${var.init_suffix}
END_INIT
}
