## unreleased


* tests/gitlab: add TF outputs for gitlab_url and registry_url
* tests/gitlab-asg: add make targets for render-ssh-config and docker-login
* tests/gitlab-asg: route port 22 thru ELB to port 8022 on the instance
* tests/gitlab-asg: switch from zone ID to use the domain name
* tests/gitlab-asg: add basic README with info on how to run the env
* init-snippet-gitlab-docker: fixup " escaping for GITLAB_OMNIBUS_CONFIG
* persistent-ebs: whitespace fixup
* persistent-ebs: update name for IAM profile/role/policy resources
* example: cloud-dev-workspace
* tests/gitlab-asg: manage SSL certs through Makefile
* tests/gitlab-asg: make the deployment more robust/complete
* init-snippet-gitlab-docker: add support for GITLAB_OMNIBUS_CONFIG
* persistent-ebs: drop old test env
* tests/vpc-scenario-2-nat-instance: fixup to sync w/ the NAT module
* prometheus-server: update to sync w/ persistent-ebs module
* single-node-asg: update to sync with persistent-ebs
* persistent-ebs: rename 'name' ---> 'name_prefix'
* tutorials/credstash: fixup module source references
* ec2-nat-instances: sync module rename to ec2-auto-recovery
* rename ec2-fixed-ip-auto-recover-instances --> ec2-auto-recover-instances
* tests/vpc-scenario-2-nate-instance: sync with ec2-nat-instance module
* tests/vpc-scenario-2-nat-instances-per-az: sync with ec2-nat-instance
* ec2-nat-instance: refactor to support auto-recovery and multiple instances
* ec2-fixed-ip-auto-recover-instances: add support for NAT
* ec2-fixed-ip-auto-recover-instances: Create N instances in M subnets
* ec2-fixed-ip-auto-recover-instances: add support for extra_tags
* ec2-fixed-ip-auto-recover-instances: parametize `aws_cloudwatch_metric_alarm`


 cloudtrail: clarify `enable_logging` parameter purpose
 cloudtrail: fix ARNs on S3 bucket policy
 cloudtrail: always use callee’s region
 cloudtrail: allow encryption of logs
 cloudtrail: add module

 ec2-nat-instance: one more test on terraform-docs
 ec2-nat-instance: test doc edit

 asg: add `placement_group` variable
 tests/gitlab-asg: sync with the gitlab-asg and init-snippet modules
 init-snippet-gitlab-docker: refactor into init_snippet
 single-node-asg: rename `name` --> `name_prefix` for consistency
 rename gitlab-single-node-asg ---> init-snippet-gitlab-docker
 leader-dns: whitespace fixup
 logstash: whitespace fixup
 ebs-snapshot: whitespace fixup
 scale-asg-iam-policy: whitespace fixup
 init-snippet-install-ops: whitespace fixup
 nat-gateways: whitespace fixup
 subnets: whitespace fixup
 init-snippet-nomad-agent: whitespace fixup
 init-snippet-install-consul: whitespace fixup
 setup-meta-infrastructure: whitespace fixup
 init-snippet-hostname: whitespace fixup
 init-snippet-hostname-simple: whitespace fixup
 init-snippet-exec: whitespace fixup
 init-snippet-curator: whitespace fixup
 init-snippet-consul-leader: whitespace fixup
 init-snippet-consul-agent: whitespace fixup
 init-snippet-attach-ebs-volume: whitespace fixup
 vpc-scenario-3: whitespace fixup
 vpc-scenario-2: whitespace fixup
 init-snippet-install-awscli: whitespace fixup
 snapshot-bucket: whitespace fixup
 vpc-scenario-1: whitespace fixup
 prometheus-server: whitespace fixup
 web-alb: whitespace fixup
 vpc-network: whitespace fixup
 vpc-legacy: whitespace fixup
 r53-subdomain: whitespace fixup
 init-snippet-prometheus: whitespace fixup
 init-snippet-write-bootstrap-pillar: whitespace fixup
 init-snippet-config-upstart-consul: whitespace fixup
 init-snippet-config-consul-leader: whitespace fixup
 elasticache-redis-cluster: whitespace fixup
 dnsmasq-server: whitespace fixup
 credstash-grant: whitespace fixup
 consul-leaders: whitespace fixup
 consul-leaders-generic-init: whitespace fixup
 single-node-asg: whitespace fixup
 nomad-agent-sg: fixup description
 mongo-server-sg: fixup description
 consul-leader-wan-sg: fixup description
 consul-leader-sg: fixup description
 consul-agent-sg: fixup description
 open-egress-sg: add `description` variable for egress rule
 open-ingress-sg: add `description` variable for ingress rule
 mongo-server-sg: fixup last commit
 consul-leader-sg: fixup last commit
 bind-server-sg: fixup last commit
 nomad-agent-worker-ports-sg: add `description` variable for ingress rules
 nomad-agent-sg: add `description` variable for ingress rules
 nomad-server-sg: add `description` variable for ingress rules
 nomad-agent-worker-ports-sg: dynamic port allocation uses ports 20000 to 32000
 ssh-sg: add `description` variable, for ingress rules
 single-port-sg: add `description` variable, for ingress rules

 mongo-server-sg: add `description` variable, for ingress rules
 etcd-server-sg: add `description` variable, for ingress rules
 consul-agent-sg: add `description` variable, for ingress rules
 consul-leader-wan-sg: drop wan_port / add `description` variables
 consul-leader-sg: add variable `description`, for ingress rules
 consul-leader-sg: drop CLI RPC port 8400 no longer in use
 bind-server-sg: add variable `description`, for ingress rules
 tests/gitlab-asg: add Makefile for full-cycle testing
 tests/gitlab-asg: update to use aws_security_group_rule resources
 tests/gitlab-asg: improve reusability of env and speed to deploy
 gitlab-single-node-asg: drop default AMI
 tests/vpc-scenario-2-nat-instance: update Makefile
 tests/vpc-scenario-2-nat-instance: refactor to use updated SG modules
 tests/vpc-scenario-2-nat-instance: parametize `vpc_cidr_block`
 tests: demo multiple NAT instances, 1 per AZ
 security-group-base: add new module
 ssh-sg: make it public by default
 open-egress-sg: refactor to use aws_security_group_rule
 open-ingress-sg: refactor to use aws_security_group_rule
 prometheus-node-exporter-sg: refactor to use aws_security_group_rule
 single-port-tcp-sg: rename module to single-port-sg
 single-port-tcp-sg: refactor to use aws_security_group_rule
 ssh-sg: refactor to use aws_security_group_rule
 nomad-server-sg: refactor to use aws_security_group_rule
 nomad-agent-worker-ports-sg: refactor to use aws_security_group_rule
 nomad-agent-sg: refactor to use aws_security_group_rule

* add this `CHANGELOG.md`
* overhaul `gitlab-asg` test env
    * add support for SSL and DNS
    * test / prove the module works:
        * can login, create repo, add files to repo, clone repo, push a docker image
    * update to sync with the init-snippet module
* refactor `gitlab-single-node-asg` module to `init-snippet-gitlab-docker`
    * just provide some shell script to run gitlab with docker, no longer a wrapper around the `single-node-asg` module

#### Refactor to use `aws_security_group_rule`

* `mongo-server-sg`
* `etcd-server-sg`
* `consul-leader-wan-sg`
* `consul-leader-sg`
* `bind-server-sg`
* `consul-agent-sg`


#### Module Variable Changes

* `persistent-ebs`: rename `name` ---> `name_prefix`


## v0.6.0

#### New modules

* `vpc-scenario-1`: add new module with VPC for AWS Network Scenario 1
* `vpc-scenario-2`: add new module with VPC for AWS Network Scenario 2
* `vpc-scenario-3`: add new module with VPC for AWS Network Scenario 3
* `vpc-scenario-4`: add new module with VPC for AWS Network Scenario 4
* `ec2-fixed-ip-auto-recover-instances`
* `ec2-nat-instance`
* `open-ingress-sg`
* A single-node autoscaling group running Gitlab
* `cloudwatch-auto-recover-existing-ec2-instance`


#### New Example / Test Env



#### Big Whitespace Reformat

`tf fmt` and whitespace fixups for bunches of modules:

* `nomad-server-sg`
* `nomad-agent-sg`
* `cluster-network`
* `consul-demo-server`
* `consul-agent-generic-init`
* `consul-cluster`
* `aws-ipsec-vpn`
* `asg`
* `bind-server-sg`
* `consul-leader-wan-sg`
* `consul-agent-sg`
* `ssh-sg`
* `vpc`
* `persistent-ebs`
* `packer-vpc`
* `consul-leader-sg`
* `packer/terraform-vpc`


#### update string-list variables from the old days

* `nomad-server-sg`: convert worker/server CIDR block variables to lists
* `nomad-agent-sg`: update `cidr_blocks` variable to a list
* `consul-leader-wan-sg`: update use of `cidr_blocks`
* `consul-agent-sg`: update `cidr_blocks` variable to a list
* `ssh-sg`: update `allowed_cidr_blocks` variable to a list
* `consul-leader-sg`: update `cidr_blocks` variable to a list


#### Module Changes

* `route-public`: drop the inline route in the public route table, module users can now add their own routes to that table
* `nomad-server-sg`: use `distinct()` to make the module more flexible, instead of forcing module users to put servers/agents in separate subnets
* `consul-leader-wan-sg`: bugfix for `wan_port`
* Corrected outputs in the `aws-ipsec-vpn` and `subnets` modules (exposed by TF v0.11.0)
* documentation updates:
    * move credstash into tutorial section
    * create section for tutorials
    * drop placeholder for saltstack docs
* `setup-meta-inf`: allow users to see password policy and change password
* tests: add test scenario for EC2 NAT instance
* `ec2-nat-instance`: fixup init script
* Fixed syntax errors in `consul-leader-wan-sg` (#35)
* .gitignore: add tf.out
* consul-leader-sg: name variable should not have a default
* packer/ubuntu-trusty: fixup description in build.sh
* persistent-ebs: export document from role policy, not profile.
* persistent-ebs: export policy document for other roles.
* A single-node autoscaling group running Nexus (#30)
* logstash: fail hard on CA creation failure.
* credstash-setup: do not assume `credstash` is installed globally.
* Packer/Kubespray: Adds consul binary to AMI
* Packer/Kubespray: Adds AMI description field
* Packer kubespray: Fix vpc part of readme
* kube-*: allow non-prefixed names for backwards compatibility.
* Adds name_prefix as input for kube-{controller,worker}-iam modules
* Moved curator setup snippet into its own module
* init-snippet/config-consul-leader: drop "run consul" from config snippet
* logstash: `terraform fmt`.
* logstash: increase ASG grace period so smaller instances work.
* logstash: ensure APT sources are up-to-date before provisioning.
* logstash: fix SG creation conflict when CIDRs overlap.
* elasticsearch: ensure nodes have up-to-date APT sources and pip.
* bind-server: accept multiple zone files as a pre-rendered folder.
* Trimmed the 'single-node-asg' module
* update kube-stack module:
    * update default cluster size for controllers
    * rename kube ELB to allow for longer names
* new init-snippet module to config upstart to run consul
29491ca init-snippet/install-consul: version bump to 0.9.3
453539c init-snippet/run-consul-server: rename to config-consul-leader
49bf2a1 ELK improvements and fixes:
beb8dea Adds missing comma
c0546a7 Packer kubespray: Adds ops tool to base ami
17b1491 Packer kubespray: Adds missing variables
980ef91 Adds "ec2:*" permissions to kube controller IAM role
8a6842c kubernetes: remove entire module, deprecated.
89e5750 logstash: update instructions for users on GovCloud.
b265992 credstash-setup: add `aws_cloud` to generalise `is_govcloud`.
64ac9b2 bind-server: add `aws_cloud` for GovCloud usage.
6abf5b7 elk-stack: `terraform fmt` module.
4f1ade9 elk-stack: remove unneeded data source for AMIs.
b3afeff credstash-setup: `terraform fmt`.
3f2acd0 elk-stack: use `ami-ubuntu` module.
5474e60 elasticsearch: add `is_govcloud` option.
a738d50 credstash-setup: add `is_govcloud` option.
502c0ee bind-server: allow usage of bastion host.
e5b344b open-ingress-sg: bugfix vpc_id reference
bfa5bf2 kube-*-iam: rename to use controller/worker terminology
eab1c77 kube-stack: add support, to assign IAM profiles to kube nodes
9ec2e4d elk-stack: ensure `elasticsearch` works on GovCloud.
a402842 elasticsearch: allow usage on GovCloud.
5280f9c credstash-setup: allow usage on GovCloud.
380ad27 credstash-setup: `fmt` module.
10b0ba8 elk-stack: fix AMIs data source.
eb88ec2 elk-stack: allow passing if environment is on GovCloud.
654d7df elk-stack: add GovCloud Ubuntu AMIs.
9b75f33 bind-server: `terraform fmt`.
308ae3a bind-server: allow bastion access.
e475dc7 Fixes invalid command in readme
9b7a310 Packer kubespray: update readme
51ea5e8 Packer kubespray: Move script out of json
36d1c53 Packer kubespray ami builder: Update docs
d1a1c94 Updates kubespray ami builder
9d25ec7 Fixed logstash elb output and brough back logstash r53 entry
7bac24e Generalize ALB:
a6ad352 Adds kube-iam module
9ecad00 tests/kube-stack-*: worker nodes get "unrestricted ingress" sg
3b4a8a2 open-egress-sg: whitespace fixup
0a07e85 add initial test env for kube-stack module
368db19 add new module: initial kube-stack w/ kubespray
97f97e4 Fixed public/private subnets mixup
23bb263 ami-ubuntu: update `owners` so GovCloud works.
d5118b8 vpc-scenario-*: document initial state.
f392ead vpn-scenario-4: fix outputs and route table association.
c65504d add new module: single port security group (TCP)
1f8ba63 add new module: etcd server security group
9af90e0 add mongo-server TF modules (for security groups)
817ad7c asg: add support for "extra_tags" variable
db1516f add init-snippet-exec module
01ca5ed packer-vpc: add Canonical's owner id for GovCloud AMI lookup
3bd22c5 snapshot-bucket: add `aws_cloud` variable
8f6e0cb dnsmasq-server: add new module
5478b69 packer-vpc: correct ami references in outputs
7d2d3fd vpc-scenario-1: add docs to vpc-scenario-1 module to demo order of operations
dec1cce vpc-scenario-*: update output references in VPC modules to sync with others
f510826 vpc: rename `id` and `cidr_block` outputs to include `vpc_` prefix
aa81d47 vpc-scenario-1: fixup cidr_blocks list in public-subnets
1f2d55b vpc-scenario-4: fixup public flag when using private-subnets module
82e7a81 vpc-scenario-3: fixup public flag when using private-subnets module
399a231 consul-demo-server: set default instance_type to t2.micro
610e359 single-node-asg: whitespace fixup
0099161 add aws_cloud variable/plumbing to consul-demo module + dependents
4dafa38 consul-demo-server: drop extra `load_balancers`
b25418e persistent-ebs: drop extra `extra_tags` variable definition
7410c32 vpc-scenario-3: fixup errors in module
86f928a cross-account-group: whitespace fixup
1d3bde3 setup-meta-infrastructure: whitespace fixup
85898e3 add aws_cloud variable to IAM modules
2380dea s3-remote-state: add aws_cloud variable
3a3d3f2 tests/vpc-scenario-2: add env to test the vpc-scenario-2 module
6822a6a tests/vpc-scenario-1: add env to test the vpc-scenario-1 module
010aacf Adds packer for kubespray-base-ami
4b36168 WIP of translating ELB to ALB for ELK:
08f7b6a WIP on moving ELB to ALB for ES and Kibana
6e24e72 Extracted Route53 attachments into separate module for ELK componenets
32fdb44 ELK improvements:
70dfdc1 Improvements to ELK
b591a32 Made master_only optional for curator. Fixed metricbeat process module name
1b861a5 Adjusted collected metrics and added outputs to be used for alerts
39b5510 Consolidated elasticsearch nodes config files into one
24be377 Update elasticsearch config
