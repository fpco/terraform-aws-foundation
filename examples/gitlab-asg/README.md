# Gitlab on AWS Single Instance Auto-scaling Groupd using Terraform

## Overview

### Current Version
Running this example creates a running instance of Gitlab with the following characteristics:

* Gitlab is running on a single EC2 instance on AWS in an auto-scaling group, so AWS will re-create the node if it fails in specific ways
* The EC2 instance is created with a fixed EIP, so the DNS record still works even if a new node is created. This obviates the need for an ELB.
* The Gitlab runner is installed on the instance via apt
* A Docker registry is also setup
* SSL certificates are created using Let's Encrypt
* Integrated with SSH
* Integrated with Route53 for DNS
* A persistent EBS volume is mounted to the instance and all gitlab data is stored on that volume - data is retained when the node is replaced

### Future additions
* Add SMTP server for password notifications

This Terraform environment also serves as a great demo of Gitlab's features and the basics of CI/CD. 

### Requirements
- Terraform 0.11.7
- A recent version of `jl` which can be downloaded from [here](https://github.com/chrisdone/jl/releases). Be sure to put the binary somewhere on your path (eg in `/usr/local/bin`) and name it `jl`.
* `envsubst` which is part of `gettext`. This already installed on most Linux systems, but if not, install the appropriate system package (`gettext` on most systems, including brew on Mac, but `gettext-base` on Debian/Ubuntu).

## Deploying the Example

### Define your Deployment
First, copy `vars.mk.sample` to `vars.mk` and review/update the variables defined there-in. In particular, set:

* `DNS_ZONE_NAME` to the one associated with your AWS account
* `REGION` to one that has some resources available to you
* `GITLAB_NAME` to the specific subdomain you want for the Gitlab URL
* `GITLAB_REGISTRY_NAME` to the specific subdomain you want for the Gitlab registry URL
* `ENVIRONMENT_NAME` to the name you want to use to label AWS elements

### Initial Deploy
Then, from the top-level code directory, run the following make targets:

```bash
ᐅ make generate-ssh-key
ᐅ make render-tfvars
ᐅ make network
ᐅ make plan
ᐅ make apply
ᐅ make render-ssh-config
```
This will generate a local SSH configuration file containing public and private host entries for the gitlab server. To use `ssh` directly, add `-F ssh_config` to the command line, eg `ssh -F ssh_config gitlab-server-sysadmin ...`. Subsequent steps below use `ssh` to access that host, so it may be worth checking that this works.

### Create EBS volume
In order for the repository data not to be lost, you need to create an external EBS volume and mount it to the `gitlab` instance. This requires some manual setup the very first time to make it available for use. The commands to do that are executed by this step:

```bash
ᐅ make init-ebs-volume
```

After running the above step to initialize the EBS, terminate the instance with the following command:

```bash
ᐅ make terminate-gitlab-server
```

The autoscaling group will bring up a new instance that will be running Gitlab once the instance is done initializing. It will use the same EIP address, so DNS and `ssh_config` will work without change. This new instance (and all subsequent ones) will have the external `EBS`  volume properly mounted in the `/gitlab` directory. Because Gitlab is running in Docker, and the `docker image` is set up to store it's data in the `/gitlab` directory, making the above change guarantees the same repository data will be used across auto-scaled instances.

However, you'll need to wait a couple of minutes for the old instance to terminate and the new one to be provisioned. You can check progress in the EC2 console if you're impatient. Once done you need to edit `~/.ssh/known_hosts` and delete the line with the EIP, so that you will be prompted again the next time you `ssh`. Otherwise `ssh` will give you an error and refuse to connect.

## Using Gitlab
Once deployed you can test the deployment and play around with Gitlab.


### Initial Setup
First you need to secure the installation:
* Open the URL to the new gitlab instance in your browser
  (equivalent to `https://$GITLAB_NAME.$DNS_ZONE_NAME/`).  
  Note that `https` is a requirement.
* Setup a password in the TOFU ("trust on first-use" workflow)
    * go to the gitlab URL with your browser
    * enter your password
* Login as `root` and use the password you just set

### Register a Runner
* Go to the *Admin area* (look for the wrench on the top menu), then *Overview -> Runners*
* Copy the Runner registration token shown on the Runners page
* In the terminal, run `make register-gitlab-runner`. You should see something like the output below. Use the responses in that output, with the following caveats:
    * Use `http://localhost` as the _gitlab-ci coordinator URL_
    * Paste the registration token copied in previous step
    * Run the `docker` executor with a default image you are happy with (we use `alpine` in this example here).

#### Example Runner Registration
```
Running in system-mode.                            
                                                   
Please enter the gitlab-ci coordinator URL (e.g. https://gitlab.com/):
http://localhost
Please enter the gitlab-ci token for this runner:
WsYr4zLeW9wZSqvK7-Md
Please enter the gitlab-ci description for this runner:
on-host-docker-executor
Please enter the gitlab-ci tags for this runner (comma separated):
docker
Whether to run untagged builds [true/false]:
[false]: true
Whether to lock the Runner to current project [true/false]:
[true]: false
Registering runner... succeeded                     runner=Wsy4UZLe
Please enter the executor: virtualbox, kubernetes, shell, ssh, parallels, docker+machine, docker-ssh+machine, docker, docker-ssh:
docker
Please enter the default Docker image (e.g. ruby:2.1):
alpine:3.7
Runner registered successfully. Feel free to start it, but if it's running already the config should be automatically reloaded!
```

#### Working with Gitlab SysAdmin commands at the CLI

Since Gitlab is running in Docker on the EC2 instance, there is a bit of indirection necessary to access the Docker CLI when you want to do Gitlab system administration work at the CLI.

First `ssh` to the EC2 instance:

```bash
ssh -F ssh_config gitlab-server-sysadmin
```

Then get the container id of the Docker container, and connect to it:

```bash
sudo docker ps   # Output displays Container ID for next step
sudo docker exec -it <container_id> /bin/bash
```
Once there you can do any of the system administration tasks described in the Gitlab manual.

## Take down the installation when finished

`make clean` will remove everything both locally and in AWS.
