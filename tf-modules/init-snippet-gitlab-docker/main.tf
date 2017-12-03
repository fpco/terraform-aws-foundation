/**
 * ## init-snippet to run gitlab w/ docker
 *
 * Document.
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

# render init script snippet from the template
data "template_file" "init_snippet" {
  template = <<END_INIT
# start snippet - run gitlab docker image
${var.init_prefix}
cmd="docker run --detach \
    --restart always \
    --publish ${var.gitlab_https_port}:443 \
    --publish ${var.gitlab_http_port}:80 \
    --publish ${var.gitlab_ssh_port}:22 \
    --volume ${var.gitlab_data_path}/config:/etc/gitlab \
    --volume ${var.gitlab_data_path}/logs:/var/log/gitlab \
    --volume ${var.gitlab_data_path}/data:/var/opt/gitlab \
    ${var.gitlab_image_repo}:${var.gitlab_image_tag}"
echo "$cmd" > /etc/rc.local
$$cmd
${var.init_suffix}
END_INIT
}

output "init_snippet" {
  value = "${data.template_file.init_snippet.rendered}"
}
