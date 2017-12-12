/**
 * ## init-snippet to run gitlab w/ docker
 *
 * Generate a `docker run` command to run the gitlab server. The command is
 * both written out to `/etc/rc.local` and to executed directly in the snippet.
 *
 */

variable "init_prefix" {
  default     = ""
  description = "initial init (shellcode) to prefix this snippet with"
}

variable "init_suffix" {
  default     = ""
  description = "init (shellcode) to append to the end of this snippet"
}

variable "log_prefix" {
  default     = "OPS: "
  description = "string to prefix log messages with"
}

variable "gitlab_domain" {
  description = "The example.com in gitlab.example.com"
  type        = "string"
}

variable "gitlab_name" {
  description = "The name of the gitlab instance, to build URL (gitlab.example.com)"
  default     = "gitlab"
  type        = "string"
}

variable "gitlab_registry_name" {
  description = "The name for the gitlab registry, without the 'gitlab.example.com'"
  default     = "registry"
  type        = "string"
}

variable "gitlab_ssh_port" {
  default     = "8022"
  description = "The port to use for ssh access to the gitlab instance"
}

variable "gitlab_http_port" {
  default     = "80"
  description = "The port to use for http access to the gitlab instance"
}

variable "gitlab_https_port" {
  default     = "443"
  description = "The port to use for https access to the gitlab instance"
}

variable "gitlab_image_tag" {
  default     = "latest"
  description = "The tag for the docker image"
}

variable "gitlab_image_repo" {
  default     = "gitlab/gitlab-ce"
  description = "The name (repo) of the docker image"
}

variable "gitlab_data_path" {
  default     = "/gitlab"
  description = "path for gitlab data"
}

# render the "GITLAB_OMNIBUS_CONFIG" envvar for inclusion in our init snippet
# NOTE - leave the '\' at the end of the rendered template, as there will be a
# newline when using rendered()
# This is opinionated, in that it's setting up SSL, nginx and the registry to
# work with SSL and the AWS ELB
data "template_file" "omnibus_config" {
  template = <<EOC
external_url '$${gitlab_url}'; registry_external_url '$${registry_url}'; registry_nginx['listen_port']=$${http_port}; registry_nginx['listen_https'] = false; registry_nginx['proxy_set_headers'] = {'X-Forwarded-Proto' => 'https', 'X-Forwarded-Ssl' => 'on'}; nginx['listen_port']=$${http_port}; nginx['listen_https'] = false; nginx['proxy_set_headers'] = {'X-Forwarded-Proto' => 'https', 'X-Forwarded-Ssl' => 'on'};
EOC

  vars = {
    gitlab_url     = "https://${var.gitlab_name}.${var.gitlab_domain}"
    registry_url   = "https://${var.gitlab_registry_name}.${var.gitlab_domain}"
    ssh_port       = "${var.gitlab_ssh_port}"
    http_port      = "${var.gitlab_http_port}"
  }
}
# render init script snippet from the template
data "template_file" "init_snippet" {
  template = <<END_INIT
# start snippet - run gitlab docker image
${var.init_prefix}
cmd="#!/bin/sh
docker run --detach \
  --restart always \
  --hostname ${var.gitlab_name}.${var.gitlab_domain} \
  --publish ${var.gitlab_https_port}:443 \
  --publish ${var.gitlab_http_port}:80 \
  --publish ${var.gitlab_ssh_port}:22 \
  --volume ${var.gitlab_data_path}/config:/etc/gitlab \
  --volume ${var.gitlab_data_path}/logs:/var/log/gitlab \
  --volume ${var.gitlab_data_path}/data:/var/opt/gitlab \
  --env GITLAB_OMNIBUS_CONFIG=\"${data.template_file.omnibus_config.rendered}\" \
  ${var.gitlab_image_repo}:${var.gitlab_image_tag}"
echo "$cmd" > /etc/rc.local
chmod +x /etc/rc.local
/etc/rc.local
${var.init_suffix}
END_INIT
}

// rendered init snippet to run gitlab with docker
output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}

// connection details about gitlab
output "gitlab_config" {
  value = {
    external_url          = "https://${var.gitlab_name}.${var.gitlab_domain}"
    registry_external_url = "https://${var.gitlab_registry_name}.${var.gitlab_domain}"
    ssh_port              = "${var.gitlab_ssh_port}"
    http_port             = "${var.gitlab_http_port}"
    https_port            = "${var.gitlab_https_port}"
  }
}
