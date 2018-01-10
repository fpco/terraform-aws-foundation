# Gitlab on AWS Single Instance Auto-scaling Groupd using Terraform

## Overview

### Current Version
Running this example creates a running instance of Gitlab with the following characteristics:

* Gitlab is running on a single EC2 instance on AWS in an auto-scaling group, so AWS will re-create the node if it fails in specific ways
* The Gitlab runner is installed on the instance via apt
* A Docker registry is also setup
* SSL certificates are used
* Integrated with SSH
* Integrated with Route53 for DNS
* A persistent EBS volume is mounted to the instance and all gitlab data is stored on that volume - data is retained when the node is replaced

### Future additions
* Add SMTP server for password notifications
* Instead of an ELB we use a fixed IP

This Terraform environment also serves as a great demo of Gitlab's features and the basics of CI/CD. 

### Requirements
- Terraform 0.11.7
- A recent version of `jl` which can be downloaded from [here](https://github.com/chrisdone/jl/releases). Be sure to put the binary in `/usr/local/bin` and name it `jl`
- `cfssl` and `cfssljson` You can download it for Linux using the `make install-cfssl` below. If you already have it installed or installed it via `brew` on your Mac, skip that make step and run the following commands in the code directory:
```
ln -s /usr/local/bin/cfssl .
ln -s /usr/local/bin/cfssljson .
```

## Deploying the Example

### Define your Deployment
First, edit `vars.env` and review/update the variables defined there-in.

### Initial Deploy
Then, from the top-level code directory, run the following make targets:

```bash
ᐅ make render-tls-configs
ᐅ make generate-ssh-key
ᐅ make install-cfssl      # see caveat above under requirements
ᐅ make generate-tls-certs
ᐅ make upload-tls-certs
ᐅ make render-tfvars
ᐅ make network
ᐅ make plan
ᐅ make apply
ᐅ make render-ssh-config
```
Add the hosts entry for SSH found in the `ssh_config` file to `~/.ssh/config`.

## Using Gitlab
Once deployed you can test the deployment and play around with Gitlab.

### Create EBS volume
In order for the repository data not to be lost, you need to create an external EBS volume and mount it to the `gitlab` instance. This requires some manual setup the very first time to make it available for use. The commands to do that are excuted by this step:

```bash
ᐅ make init-ebs-volume
```

After running the above step to initialize the EBS, terminate the instance with the following command:

```bash
ᐅ make terminate-gitlab-server
```
The autoscaling group will bring up a new instance that will be running gitlab once it is done initializing. Once you set up Gitlab as below, modify the default repository location in the gitlab configuration, according to [these instructions][1].

### Initial Setup
First you need to secure the installation:
* Open the URL to the new gitlab instance in your browser
* Setup a password in the TOFU ("trust on first-use" workflow)
    * go to the gitlab URL with your browser
    * enter your password
* Login as `root` and use the password you just set

### Register a Runner
* Go to the *Admin area* (look for the wrench on the top menu), then *Overview -> Runners*
* Copy the Runner registration token shown on the Runners page
* In the terminal, run `make register-gitlab-runner`. You should see something like the output below. Use the responses in that output, with teh following caveats:
    * Use `http://localhost` as the "gitlab-ci coortinator URL", 
    * Paste the registration token copied in previous step
    * Run the `docker` executor with a default image you are happy with (we use `alpine` in this example here).

#### Example Runner Registration
```bash
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

[1]:https://docs.gitlab.com/ee/administration/repository_storage_paths.html