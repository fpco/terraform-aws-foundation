## Creating a private Kubernetes Cluster using Terraform

### Purpose of This Example
The purpose of this example is to show you how to go through the process of building AWS infrastructure with FPCO's AWS Terraform Foundation and then install Kubernetes on that infrastructure using Kubespray.
 
### How to Use This Example
This example has two stages. In the first stage we will use FPCO Terraform modules to create a cluster on AWS. Once the cluster is up and running properly, we will use Kubespray to install Kubernetes inside that cluster. Please note, that Kubespray in the Kubernetes Incubator is a work in progress. The state of any given master branch might have a problem that causes the Kubespray installation to fail at some point. Nonetheless, the steps below should at least get to you to the point of an operational Kubernetes cluster. We hope to provide a stable version of Kubespray soon, which you can use with this example.

For the operations described below to work properly, you need valid credentials to access the resources on AWS. Use either a tool like [aws-env][1] or define the following three variables:

```bash
export AWS_ACCESS_KEY_ID="xxxxx"
export AWS_SECRET_ACCESS_KEY="yyyyy"
export REGION="<region where you put the cluster>"
```

The software required to run this example, besides Terraform, is:
* Python
* Pip - Python package manager
* [Ansible][2] - a tool for automating software deployment used by Kubespray
* [Virtualenvwrapper][3] - _Virtual environments_ is a technique used (particularly with Python) to work on more than one project at a time without introducing conflicts in their dependencies. This tool makes it easier to work with virtual environments.

`Virtualenvwrapper` requires some specific steps to install and use:

```bash
pip install virtualenvwrapper
export WORKON_HOME=~/Envs       # add this line to your .bashrc file as well
mkdir -p $WORKON_HOME
source /usr/local/bin/virtualenvwrapper.sh
```

To create an environment use the command `mkvirtualenv myenv`. To stop working on that environment issue the command `deactivate`. Then to work on that environment again, issue the command `workon myenv`. 

#### Create the cluster using FPCO Terraform modules
First, copy `vars.env.sample` to `vars.env` and edit the values. In particular it is likely necessary that you set a new value for the CoreOS AMI by choosing an HVM AMI for your particular region from the list for the stable channel found [here][4] or [here][5]. The version currently in `vars.env.sample` is likely out of date.

Then review other variables defined in `variables.tf`. You can change their values by adding corresponding values in `vars.env` and `terraform-tfvars.tpl` similar to the examples already in `vars.env.sample`.

Once you've set up your variables, run these `make` targets:

```bash
ᐅ make render-tfvars
ᐅ make generate-ssh-key
ᐅ make network
ᐅ make plan
ᐅ make apply
```

After you have successfully completed creating the cluster infrastructure, add the following commands to your ssh configuration file `~/.ssh/config`:

```
Host 10.10.*
  ProxyCommand ssh -W %h:%p bastion
  IdentityFile ~/.ssh/id_rsa
  User core
Host ip-10-10-*
  ProxyCommand ssh -W %h:%p bastion
  IdentityFile ~/.ssh/id_rsa
  User core
Host bastion
  hostname <your bastion ip here>
  User ubuntu
  IdentityFile ~/.ssh/id_rsa
  ControlMaster auto
```

Be sure to substitute the IP address for the bastion host. If you aren't sure what that is, run the command `terraform output bastion_ip`. If you used a different ssh key than the standard one, be sure to edit the config to reflect the key you actually used.

##### Install Kubernetes Using Kubespray

You are now ready to use Kubespray to install Kubernetes in the cluster you just created. To do so, take the following steps:
1. Create a virtual environment within which you will work while deploying Kubespray:
```bash
mkvirtualenv kubespray
```
2. Use `pip` to install the following Python packages: `boto3`,`os`,`json`,`argparse`,`netaddr`
2. If you don't already have it installed, use pip to install [`ansible`][6]
3. `git clone` the [Kubespray repository][7] from the Kubernetes Incubator to your local machine and then go into the created directory
4. We need to create a directory where we can define the specific parameters for our own cluster. In the `inventory` directory, copy the sample directory to a directory named after whatever you want to name your cluster, as follows:
```bash
cp -R inventory/sample/ inventory/mycluster
```
5. One set of parameters we need to create, is an inventory of how the different EC2 instances are allocated for different purposes in the Kubernetes cluster. The Terraform setup has added tags to each instance to define its role in the cluster. We will use a Python script to dynamically create the inventory list (to read more about this, see [here][8]. Link the `kubespray-aws-inventory.py` Python script into the directory you just created:
```bash
ln -s ../../contrib/aws_inventory/kubespray-aws-inventory.py inventory/mycluster/
```
6. **It is important to note the following regarding the use of the above script:** if, using this example, you deploy more than one cluster in one region, the Python inventory script is not capable of differentiating between the two.
7. To test that you properly set things up in the previous step run the following command. It should print out your inventory in the terminal, listing all the nodes along with what role(s) they have in the cluster: `etcd`,`master` and/or `node`
```bash
python inventory/mycluster/kubespray-aws-inventory.py --list
```
8. Next you need to set some parameters in the file `inventory/mycluster/group_vars/all.yml`. Uncomment these variables and/or set the values as directed below. Note that these variables are scattered throughout the file so you have to look for them. Note as well you should review the entire file since there may be other variables you might like to change:
```
bootstrap_os: coreos
apiserver_loadbalancer_domain_name: "<see note below on how to find elb name which should put here between quotes>"
# since we use an external load balancer we need to set local lb to false
loadbalancer_apiserver_localhost: false
cloud_provider: aws
etcd_memory_limit: 0
```
9. You can find the DNS name for the API server load balancer by running `terraform show` and searching for `module.kube-cluster.aws_elb.kube-controllers`. One of the parameters listed under this is `dns_name`. Copy the value into the appropriate place above. Here is how the sample output will look:
```
module.kube-cluster.aws_elb.kube-controllers:
  id = kube-stack-test-kube-api
  access_logs.# = 0
  arn = arn:aws:elasticloadbalancing:us-east-1:799914493997:loadbalancer/kube-stack-test-kube-api
  availability_zones.# = 2
  availability_zones.1126648633 = us-east-1
  availability_zones.3958969427 = us-east-1
  connection_draining = true
  connection_draining_timeout = 60
  cross_zone_load_balancing = false
  dns_name = kube-stack-test-kube-api-358192229.us-east-1.elb.amazonaws.com
```
10. Delete the file `inventory/mycluster/hosts.ini` since we are using dynamic inventory.
11. Next you need to set some parameters in the file `inventory/mycluster/group.vars/k8s-cluster.yml`. Uncomment these variables and/or set the values as directed below. Note that these variables are scattered throughout the file so you have to look for them. Note as well you should review the entire file since there may be other variables you might like to change:
```
kube_api_anonymous_auth: false
enable_network_policy: true
cluster_name: mycluster.local
resolvconf_mode: host_resolvconf
helm_enabled: true
ingress_nginx_enabled: true
kubeconfig_localhost: true
kubectl_localhost: true
```
12. We are now ready to run the ansible playbooks. You need to add the `-b` flag so scripts can take on `sudo` privileges:
```bash
ansible-playbook -b -i inventory/mycluster/kubespray-aws-inventory.py  cluster.yml
```
13. Once Kubespray is finished you need to copy the generated `kubectl` configuration to a convenient location:
```bash
cp inventory/mycluster/artifacts/admin.conf ~/.kube
```
14. You should edit the `admin.conf` you copied and substitute the DNS name you found previously for the ELB for the IP address of the `cluster` `server`. In addition change the port from `6443` to `443`. It should look something like this:
```
    server: https://kube-stack-test-kube-api-17310002321.us-east-1.elb.amazonaws.com:443
```
15. You can now check that `kubectl` is working by running the following command:
```
kubectl --kubeconfig="/<your home directory>/.kube/admin.conf" get nodes
```


[1]:	https://github.com/fpco/devops-helpers/blob/master/doc/aws/aws-env.md
[2]:	http://docs.ansible.com/ansible/latest/index.html
[3]:	https://virtualenvwrapper.readthedocs.io/en/latest/command_ref.html
[4]:	https://coreos.com/os/docs/latest/booting-on-ec2.html
[5]:	http://stable.release.core-os.net/amd64-usr/current/coreos_production_ami_all.json
[6]:	https://github.com/ansible/ansible
[7]:	https://github.com/kubernetes-incubator/kubespray/
[8]:	https://github.com/kubernetes-incubator/kubespray/blob/master/docs/aws.md
