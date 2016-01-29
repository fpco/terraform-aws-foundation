## Generic Host Init for Consul Leaders

The purpose of this module is to provide the `user_data` to init the hosts that
will make up a cluster of Consul Leaders.

This module relies on the same assumptions as the other modules in this
repository (we're using saltstack for CM and to abstract away the details). Thus,
the `user_data` generated makes use of Saltstack, the platform's configuration
management formula and related abstractions.

The goals of this module are:

* make the code easier to read and maintain,
* make the ASG definitions more reliable and consistent,
* simplify the code that defines the environment as a whole

See also the `consul-agent-generic-init` module in this repository.


### Example

```
# provision consul leaders
module "leader-init" {
    source = "../tf-modules/consul-leaders-generic-init"
    datacenter = "${var.region}"
    cidr_prefix_a = "${var.cidr_prefix_leader_a}"
    cidr_prefix_c = "${var.cidr_prefix_leader_c}"
    consul_secret_key = "${var.consul_secret_key}"
    consul_client_token = "${var.consul_master_token}"
    consul_master_token = "${var.consul_master_token}"
    # use extra_* to customize init..
    extra_pillar = "extra: pillar"
    extra_init = <<EOF
echo "customize this node's init.."
date
consul --version
salt-call --version
uname -a
EOF
    #hostname_prefix = "custom"
}
```


### Variables

* `cidr_prefix_a` - CIDR block prefix to seed IP details in the formula, subnet a.
* `cidr_prefix_c` - CIDR block prefix to seed IP details in the formula, subnet c.
* `consul_secret_key` - Secret key provided to consul, for cluster crypto.
* `consul_client_token` - Client token for services on the node, connecting to
consul as a client.
* `consul_master_token` - Master token provided to consul leader as root ACL/token.
* `leader_count` - Number of leaders to bootstrap consul cluster
* `datacenter` - Provided to consul as datacenter config, usually maps to AWS region
* `extra_pillar` - YAML to insert as pillar in bootstrap.sls.
* `extra_init` - Shell/bash to append to node init via user_data.
* `hostname_prefix` - When we update the hostname, prefix i-xxxxxx with this.
* `log_level` - Set log verbosity on node init, for CM with saltstack.


### Outputs

* `user_data` - The rendered template, using all inputs to produce the `user_data`
to pass on over to AWS.
