## Init Snippet: Run Nexus (w/ Docker)

Generate an init snippet to use Docker to run the Nexus package manager:

* use `apt-get` to install docker
* create `/etc/rc.local` to create/chown the `data_path` and run the
  docker image

The `sonatype/nexus` docker image is run.

