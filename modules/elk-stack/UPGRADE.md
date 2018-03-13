# Rolling upgrade

Sooner or later we will want to do change to our ELK that requires mutating current state of EC2
instances, be it a newer AMIs or the ELK software itself. The common approach of doing mutation and
upgrading software on running EC2 instances, such as `salt`, `ansible`, `puppet` or simple `ssh` is
not advised, our infrastructure was deployed with terrraform, that is also what we need to use in
order to make any change. As a consequence of this we will have to terminate instances, so the steps
described below will be applicable even to situations when you simply need to get beefier machines and
change EC2 instance types.

Below are the steps with all the details on how to do the upgrade without any downtime.
First you make a change to terraform configuration that will require replacement of EC2
instances. Changing ELK version, ami, instance type, or anything else that causes launch
configuration initialization script to be changed fall under catigory of rolling upgrade.

The most dangerous one is upgrading ELK version, especially if it is the major version that is being
upgraded, so we'll use that in this guide as an example and we'll be upgrading ELK version "5.6.2"
to "6.2.2". Luckily for us direct rolling upgrade is possible, which is not always the case and we
might have to upgrade to some intermedeate version, before upgrading to the desired version. [Reference
the documentation](https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-upgrade.html)
on possible version upgrades.

As it is mentioned above, before just jumping into changing the state of our ELK it is good idea to
research on possible pitfalls that can happen along the way. I can't predict and describe all of
them here, especially for Elasticsearch, since it is the most complicated component in the stack and
doing something wrong can result in data loss or corruption. During a rolling upgrade, for instance,

## Update terraform

Do the necessary update to terraform. In oor case we change `elk_version` variable to `"6.2.2"`,
which will cause versions of Elasticsearch, Logstash and Kibana to be changed. Applying the
terraform will not affect currently running EC2 instances, but only the launch configurations,so
this step has no effect on health of the cluster, unless a new instance happens to be launched right
after the change.

```bash
$ terraform apply
```

Wait for terraform to finish. After it's done, any new instances that will be created will use new
configuration and will install the newer version. So, in theory, all we need to do right now is
terminate currently running EC2 instances and let AWS autoscaling group detect the fact that we no
longer have enough of those instances and automatically deploy new ones in the place of old ones.

## ELK version

Rolling upgrade means that we want our ELK stack to continue to function while we do the
upgrade. One problem is that ELK is very sensitive to mismatched versions of their software, so it
is not only the case that sometimes we can not just directly upgrade from one version to another,
but also an earlier version of Kibana might not be fully compatible with newer version of
Elasticsearch, or vice versa. Therefore expect that some hiccups are possible during the upgrade.

### Kibana+Logstash

It is more likely that newer Kibana and Logstash will work with older Elasticsearch, so we'll update
those EC2 instances first. Considering that those instances are stateless all needs to be done is
from AWS console (or aws-cli if you wish):

* teminate one kibana+logstash EC2 instance and wait for it to go through the "running ->
  shutting-down -> terminated" state transitions.
* wait for new one to be created and fully initialized, best way to check that is to inspect the
  logs the running Kibana.

In our example, sure enough, after Kibana+Lohstash node was upgraded we get this error:

> This version of Kibana requires Elasticsearch v6.2.2 on all nodes.

So all we can do now for bringing Kibana back to life is complete the upgrade on the whole
clusterand hope that logstash isn't that picky and it continues to work, which we'll know later by
inspecting the logs.

### Elasticsearch master nodes

Now that have our Kibana+Logstash upgraded let's check current version of Elasticsearch on all
running nodes:

```bash
$ curl -s https://user:pass@elasticsearch.example.com:9201/_nodes | jq '.nodes | map_values({name: .name, version: .version, roles: .roles})'
...
  "F8Pn-IGUQSWl6CGVDNzIUg": {
    "name": "elk-dev-master-node-00-us-east-1a",
    "version": "5.6.2",
    "roles": [
      "master"
    ]
  },
  "aadtHKykSeWQP7kAODSwyA": {
    "name": "elk-dev-data-node-00-us-east-1a",
    "version": "5.6.2",
    "roles": [
      "data",
      "ingest"
    ]
  },
...
```

Not sure what's the best order, but I think upgrading data nodes is the next logical step, although
I'd expect going for master nodes first should work too. Either way, when upgrading Elasticsearch
nodes, currently elected master should be upgraded __last__, since when the new master is elected,
it will be an already upgraded node.

```
$ curl -s https://user:pass@elasticsearch.example.com:9201/_cat/nodes?v
172.31.128.122  7 92 0 0.00 0.00 0.00 m  * elk-dev-master-node-00-us-east-1a
172.31.128.83  51 97 3 0.00 0.00 0.00 di - elk-dev-data-node-00-us-east-1a
172.31.129.51  47 96 4 0.02 0.04 0.00 di - elk-dev-data-node-01-us-east-1b
```

Current master is the one with the star next to it. Simply terminate one master eligible node at a
time and wait for the new one to re-appear in the above list. If all goes smoothly, only when
upgrading currently elected master there will be a short delay in API requests.

Important to note, that it is safe to terminate nodes like that with the current setup, just because
the actual data/files that are being used by the nodes is stored on a separately attached EBS volume
dedicated to each node. Which means that when either one of the master or data nodes dies, the data
is persisted and is re-attached to the newly created node. Moreover having at least 3 master and 2
data nodes gives us High Availability (HA), so cluster will continue being operational with at most
one data and one master nodes being dead at the same time.




# Scaling

## Logstash/Kibana

Increasing the number of Logstash/Kibana nodes is super easy. Simply change the terraform variables

```bash
$ terraform apply
...
Terraform will perform the following actions:

  ~ module.elk.module.logstash-kibana.aws_autoscaling_group.logstash-asg
      desired_capacity: "1" => "2"
      max_size:         "1" => "2"
      min_size:         "1" => "2"
```

And wait for new instances to popup.

## Elasticsearch

With Elasticsearch it is much trickier. Master nodes should only be scaled from 3 to 5, which has to
do with split brain problem and cannot be done without any down time. The proper way to change the
number of master nodes is to either place the whole cluster into a [readonly
state](https://www.elastic.co/guide/en/elasticsearch/reference/current/misc-cluster.html#cluster-read-only)
or completely shut it shutdown. Only after that settings can be changed and all instances re-deployed as it is described in the above [Rolling upgrade](#rolling_upgrade) section.


Data nodes can be added safely at runtime, which is simply a matter of increasing the value of
`elasticsearch_data_node_count`. It is important to understand though, that this operation is better
done during the time of a lowest load on the cluster, since adding a data node will trigger shard
rebalancing and will incur a lot of network and EBS volume IO.
