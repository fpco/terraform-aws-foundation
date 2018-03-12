## Init Snippet: Install Consul

Generate an init snippet to install the AWS cli tool on boot. This will:

* wget the consul zip file based on the version and base url variables
* unzip the archive to `/usr/local/bin/`
* run `consul version` to confirm we can see/run the executable
* add a `consul` user with `useradd`, with its home set to `data_dir`

**NOTE: this module does not validate the release checksum or setup consul
to run as a system service.

