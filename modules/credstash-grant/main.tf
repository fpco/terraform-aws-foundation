/**
 * ## Credstash Grant
 *
 * This module will make it possible for anybody assuming the supplied IAM Role to read
 * and/or write secrets from/to credstash store.
 *
 * The cleanup actions `when = "destroy"` in this module require at least
 * terraform 0.9.0.
 * See [this RFC](https://docs.google.com/document/d/15nEcV7fxskDgYrXoNMl6RYIo10PCiZGle7TP8xitrFE/)
 * and [this ticket](https://github.com/hashicorp/terraform/issues/386) for more
 * details on that.
 *
 */

variable "kms_key_arn" {
  description = "ARN for the KMS Master key created by 'credstash-setup' module"
}

// Roles count is only necessary to circumvent [terraform #4149](https://github.com/hashicorp/terraform/issues/4149) issue.
variable "roles_count" {
  description = "Number of roles that will be used during a grant process, i.e. how many roles_names there is."
}

variable "roles_names" {
  type        = "list"
  description = "Role Name for which reading and/or writing of secrets will be enabled. Must correspond 1:1 with roles_arns"
}

variable "roles_arns" {
  type        = "list"
  description = "Role ARN for which reading and/or writing of secrets will be enabled. Must correspond 1:1 with roles_names"
}

variable "reader_policy_arn" {
  default     = ""
  description = "Secrets Reader Policy ARN that was created by 'credstash-setup' module. Reading will be disabled if not supplied."
}

variable "writer_policy_arn" {
  default     = ""
  description = "Secrets Writer Policy ARN that was created by 'credstash-setup' module. Writing will be disabled if not supplied."
}

variable "reader_context" {
  default     = ""
  description = "Optional space separated contex key/value pairs that are required to read encrypted values. Eg. env=dev svc=db"
}

variable "writer_context" {
  default     = ""
  description = "Optional space separated contex key/value pairs that will be used to encrypt values with credstash. Eg. env=dev svc=db"
}

resource "aws_iam_role_policy_attachment" "credstash-reader-policy-attachment" {
  count      = "${var.reader_policy_arn == "" ? 0 : var.roles_count}"
  role       = "${var.roles_names[count.index]}"
  policy_arn = "${var.reader_policy_arn}"

  provisioner "local-exec" {
    command = "${path.module}/grant.sh create reader ${var.reader_context == "" ? "" : "--context ${join(",", split(" ", var.reader_context))}"} ${var.kms_key_arn} ${var.roles_arns[count.index]}"
  }

  provisioner "local-exec" {
    when       = "destroy"
    on_failure = "continue"
    command    = "${path.module}/grant.sh revoke ${var.kms_key_arn} ${var.roles_arns[count.index]}"
  }
}

resource "aws_iam_role_policy_attachment" "credstash-writer-policy-attachment" {
  count      = "${var.writer_policy_arn == "" ? 0 : var.roles_count}"
  role       = "${var.roles_names[count.index]}"
  policy_arn = "${var.writer_policy_arn}"

  provisioner "local-exec" {
    command = "${path.module}/grant.sh create writer ${var.writer_context == "" ? "" : "--context ${join(",", split(" ", var.writer_context))}"} ${var.kms_key_arn} ${var.roles_arns[count.index]}"
  }

  provisioner "local-exec" {
    when       = "destroy"
    on_failure = "continue"
    command    = "${path.module}/grant.sh revoke ${var.kms_key_arn} ${var.roles_arns[count.index]}"
  }
}
