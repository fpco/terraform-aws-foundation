data "template_file" "application-user-data" {
  vars = {
    setup_repos = "${data.template_file.}"
  }

  template = "${file("${path.module}/templates/application-user-data.sh")}"
}

module "auto-scaling-group" {
  source             = "../../modules/asg"
  name_prefix        = "${var.name}"
  key_name           = ""
  ami                = "${module.ami-ubuntu.id}"
  min_nodes          = 1
  max_nodes          = 5
  desired_capacity   = 1
  public_ip          = false
  azs                = "${data.aws_availability_zones.available.names}"
  subnet_ids         = "${module.public-subnets.ids}"
  security_group_ids = []
}
