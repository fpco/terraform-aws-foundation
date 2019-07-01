output "kms_key_arn" {
  value       = aws_kms_key.credstash-key.*.arn
  description = "KMS Key ARN. It can later be used to store and retrieve secrets."
}

output "kms_key_id" {
  value       = aws_kms_key.credstash-key.*.key_id
  description = "KMS Master key id."
}

output "kms_key_alias" {
  value       = aws_kms_alias.credstash-key.*.name
  description = "KMS Master key alias. It can later be used to store and retrieve secrets."
}

output "kms_key_alias_arn" {
  value       = aws_kms_alias.credstash-key.*.arn
  description = "KMS Master key alias ARN."
}

output "db_table_arn" {
  value       = aws_dynamodb_table.credstash-db.*.arn
  description = "DynamoDB table ARN that can be used by credstash to store/retrieve secrets."
}

output "db_table_name" {
  value       = aws_dynamodb_table.credstash-db.*.id
  description = "DynamoDB table name that can be used by credstash to store/retrieve secrets."
}

output "install_snippet" {
  value       = data.template_file.credstash-install-snippet.rendered
  description = "Ubuntu bash script snippet for installing credstash and its dependencies"
}

output "get_cmd" {
  value       = data.template_file.credstash-get-cmd.rendered
  description = "Credstash get command with region and table values set."
}

output "put_cmd" {
  value       = data.template_file.credstash-put-cmd.rendered
  description = "Credstash put command with region, table and kms key values set."
}

output "reader_policy_arn" {
  value       = aws_iam_policy.reader-policy.*.arn
  description = "Secret Reader policy"
}

output "writer_policy_arn" {
  value       = aws_iam_policy.writer-policy.*.arn
  description = "Secret Writer policy"
}

