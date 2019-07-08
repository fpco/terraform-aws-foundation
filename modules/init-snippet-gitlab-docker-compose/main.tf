/**
 * ## init-snippet to run gitlab w/ docker-compose
 *
 * Generate a `docker-compose.yml` file to run the gitlab docker container.
 * The file is both written out to `~/docker-compose.yml` and executed
 * using `docker-compose up` directly after in the snippet.
 *
 * Works only on Ubuntu 18.04
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

variable "gitlab_registry_bucket_name" {
  description = "the name of the S3 bucket that gitlab should be configured to write docker images to"
  type        = "string"
}

variable "gitlab_registry_bucket_region" {
  description = "the region of the S3 bucket to write docker images to"
  default     = "us-east-1"
  type        = "string"
}

variable "gitlab_fqdn" {
  description = "The full gitlab.example.com"
  type        = "string"
}

variable "gitlab_registry_subdomain" {
  description = "The name for the gitlab registry, e.g. 'registry' for 'registry.gitlab.example.com', without the 'gitlab.example.com'"
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

variable "gitlab_docker_compose_yml_dir" {
  default = "/home/ubuntu"
  description = "Absolute path to write the gitlab docker-compose.yml file to"
}

# render init script snippet from the template
data "template_file" "init_snippet" {
  template = <<END_INIT
# start snippet - run gitlab docker container with docker-compose
${var.init_prefix}

# Upgrading requests for use with docker-compose.
# Necessary in some situations
pip install --upgrade requests
apt install docker.io docker-compose -y

docker_compose="
gitlab:
  image: ${var.gitlab_image_repo}:${var.gitlab_image_tag}
  container_name: gitlab
  restart: always
  hostname: ${var.gitlab_fqdn}
  environment:
    GITLAB_OMNIBUS_CONFIG: |
      external_url 'https://${var.gitlab_fqdn}'
      nginx['listen_port'] = 80;
      nginx['listen_https'] = false;
      nginx['proxy_set_headers'] = {
        'X-Forwarded-Proto' => 'https',
        'X-Forwarded-Ssl' => 'on'
      };
      registry_external_url 'https://${var.gitlab_registry_subdomain}.${var.gitlab_fqdn}';
      registry_nginx['listen_port'] = 80;
      registry_nginx['listen_https'] = false;
      registry_nginx['proxy_set_headers'] = {
        'X-Forwarded-Proto' => 'https',
        'X-Forwarded-Ssl' => 'on'
      };
      registry['storage']= {
        's3' => {
          'bucket' => '${var.gitlab_registry_bucket_name}',
          'region' => '${var.gitlab_registry_bucket_region}'
        }
      };
  ports:
    - '${var.gitlab_http_port}:80'
    - '${var.gitlab_https_port}:443'
    - '${var.gitlab_ssh_port}:22'
  volumes:
    - '${var.gitlab_data_path}/config:/etc/gitlab'
    - '${var.gitlab_data_path}/logs:/var/log/gitlab'
    - '${var.gitlab_data_path}/data:/var/opt/gitlab'
"
echo "$docker_compose" > ${var.gitlab_docker_compose_yml_dir}/docker-compose.yml

docker-compose -f ${var.gitlab_docker_compose_yml_dir}/docker-compose.yml up -d

${var.init_suffix}
END_INIT
}

output "init_snippet" {
  value       = "${data.template_file.init_snippet.rendered}"
  description = "rendered init snippet to run gitlab with docker"
}

output "gitlab_config" {
  value = {
    external_url           = "https://${var.gitlab_fqdn}"
    registry_external_url  = "https://${var.gitlab_registry_subdomain}.${var.gitlab_fqdn}"
    registry_bucket_region = "${var.gitlab_registry_bucket_region}"
    registry_bucket_name   = "${var.gitlab_registry_bucket_name}"
    ssh_port               = "${var.gitlab_ssh_port}"
    http_port              = "${var.gitlab_http_port}"
    https_port             = "${var.gitlab_https_port}"
  }

  description = "connection details about gitlab"
}
