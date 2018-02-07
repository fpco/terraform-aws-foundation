## Gitlab on AWS ASG w/ Terraform

### What is this


### How to use

First, edit `project.env` and review/update the variables defined there-in.

Then, we run some make targets:

```
ᐅ make render-tls-configs
ᐅ make render-tfvars
ᐅ make generate-ssh-key
ᐅ make install-cfssl
ᐅ make generate-tls-certs
ᐅ make upload-tls-certs
ᐅ make network
ᐅ make plan
ᐅ make apply
ᐅ make render-ssh-config
```

Add the hosts entry for SSH to `~/.ssh/config`.

Then, you will want to do the following:

* open the URL to the new gitlab instance in your browser
* setup a password in the TOFU ("trust on first" use workflow)
* login as `root` and that password you just set
* create a new project
* add a file to the project, commit to `master`
* confirm the clone URLs look correct
* switch to the SSH key
* add an SSH pubkey to your profile
* test the repo, clone it to your localhost over SSH and then HTTPS


#### Integrated Docker Image Registry

Docker login:

```
ᐅ docker login registry.dev-sandbox.fpcomplete.com
Username: root
Password: 
Email: 
WARNING: login credentials saved in /home/user/.docker/config.json
Login Succeeded
```

Tag & push an image:

```
ᐅ docker tag 3c82203aab43 registry.dev-sandbox.fpcomplete.com/root/my-awesome-project:foobar
ᐅ docker push registry.dev-sandbox.fpcomplete.com/root/my-awesome-project:foobar
The push refers to a repository [registry.dev-sandbox.fpcomplete.com/root/my-awesome-project] (len: 1)
3c82203aab43: Pushed 
b2189dc83d7f: Pushed 
foobar: digest: sha256:3e8bfefbc2da87ef3b459db4c5edb4fb87410c5a4d8d411b98d086b009f380db size: 2152
```
