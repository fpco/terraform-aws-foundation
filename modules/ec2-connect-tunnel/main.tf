module "asg" {
  source  = "../single-node-asg"

  region    = var.region
  ami       = var.ami
  key_name  = ""
  instance_type = var.instance_type
  name_prefix   = var.name_prefix
  name_suffix   = var.name_suffix

  security_group_ids = [module.tunnel-sg.id]
  subnet_id          = var.subnet_id
  data_volumes       = []
  assign_eip         = true

  init_suffix = <<END_INIT_SUFFIX
echo "Installing ec2-instance-connect"
apt install ec2-instance-connect
END_INIT_SUFFIX
}
