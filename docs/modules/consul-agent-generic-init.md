## Generic Host Init for Consul Agents

The purpose of this module is to provide the `user_data` to init a new Host that
will be joining a network or SOA cluster built on Consul.

This module relies on the same assumptions as the other modules in this
repository (we're using saltstack for CM and to abstract away the details). Thus,
the `user_data` generated makes use of Saltstack, the platform's configuration
management formula and related abstractions.

The goals of this module are:

* make the code easier to read and maintain,
* make the ASG definitions more reliable and consistent,
* simplify the code that defines the environment as a whole


### Example

```
# provisioning for worker cluster
module "worker-init" {
    source = "../tf-modules/consul-agent-generic-init"
    region = "${var.region}"
    service = "worker"
    consul_secret_key = "${var.consul_secret_key}"
    consul_client_token = "${var.consul_master_token}"
    leader_dns = "${module.consul-leaders.leader_dns}"
    extra_pillar = "extra: pillar"
    extra_init = <<EOF
echo "customize this node's init.."
date
consul --version
salt-call --version
uname -a
EOF
}
```


### Variables

* `service` - Primary role serviced by nodes in this cluster, used in setting
the hostname for each node provisioned with this init.
* `consul_secret_key` - Secret key provided to consul, for cluster crypto.
* `consul_client_token` - Client token for services on the node, connecting to
consul as a client.
* `leader_dns` - DNS to find consul leaders to follow/join.
* `region` - The AWS region, provided to consul as datacenter.
* `extra_pillar` - YAML to insert as pillar in bootstrap.sls.
* `extra_init` - shell/bash to append to node init via user_data.
* `log_level` - set log verbosity on node init, for CM with saltstack.


### Outputs

* `user_data` - The rendered template, using all inputs to produce the `user_data`
to pass on over to AWS.
