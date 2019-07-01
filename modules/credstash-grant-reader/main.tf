variable "kms_key_arn" {
  description = "ARN for the KMS Master key created by 'credstash-setup' module"
  type        = string
}

// Roles count is only necessary to circumvent [terraform #4149](https://github.com/hashicorp/terraform/issues/4149) issue.
variable "role_count" {
  description = "Number of roles that will be used during a grant process, i.e. how many roles_names there is."
  type        = string
}

variable "role_names" {
  type        = list(string)
  description = "Role Name for which reading secrets will be enabled. Must correspond 1:1 with roles_arns"
}

variable "role_arns" {
  type        = list(string)
  description = "Role ARN for which reading secrets will be enabled. Must correspond 1:1 with roles_names"
}

variable "reader_policy_arn" {
  description = "Secrets Reader Policy ARN that was created by 'credstash-setup' module"
  type        = string
}

variable "context_keys" {
  default     = []
  description = "list of keys to be zipped with the context_values to set an 'encryption context' for additional granularity that clients are required to provide to read encrypted values. Eg. for env=dev svc=db, this would be [env, svc]. All readers get this context map."
  type        = list(string)
}

variable "context_values" {
  default     = []
  description = "list of values to be zipped with the context_keys to set an 'encryption context' for additional granularity that clients are required to provide to read encrypted values. Eg. for env=dev svc=db, this would be [dev, db]. All readers get this context map."
  type        = list(string)
}

resource "aws_iam_role_policy_attachment" "credstash-reader-policy-attachment" {
  count      = var.role_count
  role       = var.role_names[count.index]
  policy_arn = var.reader_policy_arn
}

resource "aws_kms_grant" "credstash-reader" {
  count             = var.role_count
  name              = "${var.role_names[count.index]}-credstash-reader"
  grantee_principal = var.role_arns[count.index]
  key_id            = var.kms_key_arn
  operations        = ["Decrypt"]

  constraints {
    encryption_context_equals = {
      element(var.context_keys, count.index) = element(var.context_values, count.index)
    }
  }
}

