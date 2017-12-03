/**
 * ## Init Snippet: Curator
 *
 * Document.
 *
 */

data "template_file" "curator-setup" {
  template = "${file("${path.module}/snippet.tpl.sh")}"

  vars {
    index_retention_period = "${var.index_retention_period}"
    extra_curator_actions  = "${var.extra_curator_actions}"
    elasticsearch_host     = "${var.elasticsearch_host}"
    elasticsearch_port     = "${var.elasticsearch_port}"
    master_only            = true
  }
}

variable "index_retention_period" {
  default     = 60
  description = "Age of Elasticsearch indices in days before they will be considered old and be pruned by the curator. Set to 0 in order to disable."
}

variable "extra_curator_actions" {
  default     = ""
  description = "YAML formatted dictionary of actions, as described in documentation, but started at index '2', since action number '1' is the one that purges old indices."
}

variable "elasticsearch_host" {
  default     = "localhost"
  description = "Hostname for Elasticsearch API"
}

variable "elasticsearch_port" {
  default     = 9200
  description = "Port number for Elasticsearch API"
}

variable "master_only" {
  default     = true
  description = "If installed on master eligible nodes, will only run if current node is an elected master"
}

output "init_snippet" {
  value = "${data.template_file.curator-setup.rendered}"
}
