// Ubuntu bash script snippet for installing credstash
output "credstash_install_snippet" {
  value = "${data.template_file.credstash-install-snippet.rendered}"
}
// Secret Reader policy
output "reader_policy_arn" {
  value = "${aws_iam_policy.reader-policy.arn}"
}
// Secret Writer policy
output "writer_policy_arn" {
  value = "${aws_iam_policy.writer-policy.arn}"
}

data "template_file" "credstash-install-snippet" {
  template = <<END_TEMPLATE
apt-get update
apt-get install build-essential libssl-dev libffi-dev python-dev
pip install --upgrade pip
pip install credstash
END_TEMPLATE
}
