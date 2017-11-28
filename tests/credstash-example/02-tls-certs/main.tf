variable "name" {
  description = "the name of the project"
  default     = "example-project"
}

variable "client_name" {
  description = ""
}

variable "domain_name" {
  description = ""
}

variable "certstrap_depot_path" {
  description = ""
}

variable "certstrap_ca_common_name" {
  description = ""
}

variable "credstash_put_cmd" {
  description = ""
}

variable "server_context" {
  description = ""
}

variable "client_context" {
  description = ""
}

data "template_file" "tls-bootstrap" {
  template = "${file("${path.module}/generate-certs.tpl.sh")}"
  vars {
    client_name       = "${var.client_name}" # client name ???
    domain_name       = "${var.domain_name}" # Server domain name
    depot_path        = "${var.certstrap_depot_path}" # Local path for certificates
    ca_common_name    = "${var.certstrap_ca_common_name}" # CA name
    credstash_put_cmd = "${var.credstash_put_cmd}"
    server_context    = "env=server"
    client_context    = "env=client"
  }
}

resource "null_resource" "tls-bootstrap" {
  provisioner "local-exec" {
    command = "${data.template_file.tls-bootstrap.rendered}"
  }
}
