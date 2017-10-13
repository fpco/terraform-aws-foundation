data "aws_caller_identity" "current" {}

data "aws_region" "current" {
  current = true
}

data "template_file" "credstash-get-cmd" {
  template = "env credstash -r ${data.aws_region.current.name} -t ${var.db_table_name} get"
}

data "template_file" "credstash-put-cmd" {
  template = "env credstash -r ${data.aws_region.current.name} -t ${var.db_table_name} put -k ${aws_kms_alias.credstash-key.name}"
}

data "template_file" "credstash-install-snippet" {
  template = <<END_TEMPLATE
{ apt-get update;
  apt-get install -y build-essential libssl-dev libffi-dev python-dev python-pip;
  pip install --upgrade pip;
  pip install credstash; }
END_TEMPLATE
}
