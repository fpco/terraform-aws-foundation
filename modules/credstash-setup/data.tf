data "aws_caller_identity" "current" {
}

data "aws_region" "current" {
}

data "aws_partition" "current" {
}

data "template_file" "credstash-get-cmd" {
  template = "env credstash -r ${data.aws_region.current.name} -t ${var.db_table_name} get"
}

data "template_file" "credstash-put-cmd" {
  template = "env credstash -r ${data.aws_region.current.name} -t ${var.db_table_name} put -k ${aws_kms_alias.credstash-key[0].name}"
}

data "template_file" "credstash-install-snippet" {
  template = <<END_TEMPLATE
{ apt-get update;
  apt-get install -y build-essential libssl-dev libffi-dev python-dev python-pip;
  pip install --upgrade pip==9.0.3;
  pip install credstash; }
END_TEMPLATE

}

# Writer Policy
data "aws_iam_policy_document" "writer-policy" {
  statement {
    effect = "Allow"
    actions = ["dynamodb:PutItem"]

    resources = [
      "arn:${data.aws_partition.current.partition}:dynamodb:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:table/${var.db_table_name}",
    ]
  }
}

# Reader Policy
data "aws_iam_policy_document" "reader-policy" {
  statement {
    effect = "Allow"

    actions = [
      "dynamodb:GetItem",
      "dynamodb:Query",
      "dynamodb:Scan",
    ]

    resources = [
      "arn:${data.aws_partition.current.partition}:dynamodb:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:table/${var.db_table_name}",
    ]
  }
}

