## init-snippet to run gitlab w/ docker-compose

Generate a `docker-compose.yml` file to run the gitlab docker container.
The file is both written out to `${gitlab_docker_compose_yml_dir}/docker-compose.yml` and executed
using `docker-compose up` directly after in the snippet.

NOTE: Works only on Ubuntu 18.04 (docker-compose is not available out-of-the-box on earlier LTS)
