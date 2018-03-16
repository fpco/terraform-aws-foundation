# Rolling upgrade

Sooner or later we will want to apply a change to our ELK stack that requires a mutation to the
current state of EC2 instances, be it a newer AMIs or the ELK software itself. The most common
approach is to do mutation and upgrade software right on the running EC2 instances, with tools such
as `salt`, `ansible`, `puppet` or simple `ssh`, neither of ehcih is advised. Our infrastructure was
deployed with terrraform, that is also what we must use in order to make any change. As a
consequence of this we will have to terminate instances, so the steps described below will be
applicable even to situations when you simply need to get beefier machines and change EC2 instance
types.

Below are the steps with all the details on how to do the upgrade without any downtime.
First you make a change to terraform configuration that will require replacement of EC2
instances. Changing ELK version, ami, instance type, or anything else that causes launch
configuration initialization script to be changed fall under catigory of rolling upgrade.

The most dangerous part is upgrading ELK version, especially if it is the major version that is
being upgraded. This is what we'll do in this guide as an example and we'll be upgrading ELK
version "5.6.2" to "6.2.2". Luckily for us direct rolling upgrade is possible, which is not always
the case and we might have to upgrade to some intermediate version, before upgrading to the desired
one. [Reference the
documentation](https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-upgrade.html)
on possible version upgrades.

Before jumping the gun and starting changing our ELK it is a good idea to research for possible
pitfalls that can happen along the way. I can't predict and describe all of them here, especially
for Elasticsearch, since it is the most complicated component in the stack and doing something wrong
can result in data loss or corruption.

__Important__ - Therefore, make sure you do have __backups__ of your data, either snapshots of EBS
volumes or indices backed up by curator or some other means. Exporting your Kibana dashboards and
visualizations as JSON files is also a good idea.

## Update terraform

Do the necessary update to terraform. In this example we change `elk_version` variable to `"6.2.2"`,
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
longer have enough of those instances and automatically deploy new ones in the place of old
ones. Not too fast, it must be done in the specific order.

## ELK version

Rolling upgrade means that we want our ELK stack to continue to function while we do the
upgrade. One problem is that ELK is very sensitive to mismatched versions of their software, so it
is not only the case that sometimes we can not just directly upgrade from one version to another,
but also an earlier version of Kibana might not be fully compatible with newer version of
Elasticsearch, or vice versa. Therefore expect that some hiccups are possible during the upgrade.

## Elasticsearch data nodes

Now that have our Launch cinfiguration updated let's check current version of Elasticsearch on all
running nodes:

```bash
$ curl -s https://user:pass@elasticsearch.example.com:9201/_nodes | jq '.nodes | map_values({name: .name, version: .version, roles: .roles})'
{
  "GUu-aBBOQbKDNYwUUh6U7g": {
    "name": "elk-dev-data-node-01-us-east-1b",
    "version": "5.6.2",
    "roles": [
      "data",
      "ingest"
    ]
  },
  "G5Whgk2XR3Sxv8h3jGRyTQ": {
    "name": "elk-dev-master-node-01-us-east-1b",
    "version": "5.6.2",
    "roles": [
      "master"
    ]
  },
...
```

Not sure what's the best order here, but I think upgrading data nodes is the next logical step,
although I'd expect going for master nodes first should work too. Either way, when upgrading
Elasticsearch nodes, currently elected master should be upgraded __last__, just because when it gets
killed and there is an election, it will be an already upgraded node that gets elected. Current
master is the one with the star next to it:

```
$ curl -s https://user:pass@elasticsearch.example.com:9201/_cat/nodes
172.31.132.71  32 96 3 0.00 0.02 0.02 di - elk-dev-data-node-01-us-east-1b
172.31.132.60   8 92 0 0.00 0.00 0.00 m  * elk-dev-master-node-01-us-east-1b
172.31.131.21  40 96 3 0.21 0.20 0.18 di - elk-dev-data-node-00-us-east-1a
172.31.131.162  9 92 0 0.00 0.00 0.00 m  - elk-dev-master-node-00-us-east-1a
172.31.133.118  6 93 0 0.00 0.00 0.00 m  - elk-dev-master-node-02-us-east-1c
```

Important to note, that it is safe to terminate nodes with the current setup, just because the
actual data is stored on a separately attached EBS volume dedicated to each node. Which means that
when either one of the master or data nodes dies, the data is persisted and is re-attached to the
newly created node. Moreover having at least 3 master and 2 data nodes gives us High Availability
(HA), so cluster will continue being operational with at most one data and one master nodes being
dead at the same time.

Now we will upgrade data nodes. In order to keep the upgrade "rolling", we need to make sure not to
get our cluster in a `"red"` status, therefore it is important to follow the [official
guide](https://www.elastic.co/guide/en/elasticsearch/reference/current/rolling-upgrades.html) and
not to take down more than one node at a time.

* Turn off allocation:

```
$ REQ='{"persistent": {"cluster.routing.allocation.enable": "none"}}'
$ curl -s -XPUT https://user:pass@elasticsearch.example.com:9201/_cluster/settings?pretty -d "$REQ" -H 'Content-Type: application/json'
```

* Perform synced flush:

```
$ curl -s -XPOST https://user:pass@elasticsearch.example.com:9201/_flush/synced
```

* Terminate one data node either through AWS Console or with `aws-cli` and wait for a new one to be
  created in it's place. You can check the status of your cluster while the data node is being
  replaced, it should be place the cluser in `"yellow"` status and the number of data nodes will be
  one less than usual:

```
$ curl -s https://user:pass@elasticsearch.example.com:9201/_cluster/health?pretty
{
  "cluster_name" : "elasticsearch",
  "status" : "yellow",
  "timed_out" : false,
  "number_of_nodes" : 4,
  "number_of_data_nodes" : 1,
  "active_primary_shards" : 31,
  "active_shards" : 31,
  "relocating_shards" : 0,
  "initializing_shards" : 0,
  "unassigned_shards" : 31,
  "delayed_unassigned_shards" : 0,
  "number_of_pending_tasks" : 0,
  "number_of_in_flight_fetch" : 0,
  "task_max_waiting_in_queue_millis" : 0,
  "active_shards_percent_as_number" : 50.0
}
```

* Once the new node is discovered by Elasticsearch, we should turn allocation back on and wait for
  all unassigned shards to become active again, which will also result in `"green"` status.

```
$ REQ='{"persistent": {"cluster.routing.allocation.enable": "all"}}'
$ curl -s -XPUT https://user:pass@elasticsearch.example.com:9201/_cluster/settings?pretty -d "$REQ" -H 'Content-Type: application/json'
{
  "acknowledged" : true,
  "persistent" : {
    "cluster" : {
      "routing" : {
        "allocation" : {
          "enable" : "all"
        }
      }
    }
  },
  "transient" : { }
}
$ # give it some time periodically checking for status until "green":
$ curl -s https://user:pass@elasticsearch.example.com:9201/_cluster/health?pretty
{
  "cluster_name" : "elasticsearch",
  "status" : "green",
  "timed_out" : false,
  "number_of_nodes" : 5,
  "number_of_data_nodes" : 2,
  "active_primary_shards" : 31,
  "active_shards" : 62,
  "relocating_shards" : 0,
  "initializing_shards" : 0,
  "unassigned_shards" : 0,
  "delayed_unassigned_shards" : 0,
  "number_of_pending_tasks" : 0,
  "number_of_in_flight_fetch" : 0,
  "task_max_waiting_in_queue_millis" : 0,
  "active_shards_percent_as_number" : 100.0
}
```

As we can see, one of our data nodes was successfully upgraded with no down time:

```
$ curl -s https://user:pass@elasticsearch.example.com:9201/_nodes | jq '.nodes | map_values({name: .name, version: .version, roles: .roles})'
{
  "G5Whgk2XR3Sxv8h3jGRyTQ": {
    "name": "elk-dev-master-node-01-us-east-1b",
    "version": "5.6.2",
    "roles": [
      "master"
    ]
  },
  "GUu-aBBOQbKDNYwUUh6U7g": {
    "name": "elk-dev-data-node-01-us-east-1b",
    "version": "6.2.2",
    "roles": [
      "data",
      "ingest"
    ]
  },
...
```

We repeat above process for every data node one at a time.

## Elasticsearch master nodes

When we finish upgrading all of our data nodes it is time we switch to upgrading master eligible
nodes. As mentioned in previous section, it is better to upgrade the currently elected master at the
end. Besides that, nothing special needs to be done here, we simply terminate one master eligible
node at a time and wait for the new one to re-appear in the list of cluster nodes whan calling
`/_cat/nodes`. If all goes smoothly, only when upgrading currently elected master there will be a
short delay in API requests, which corresponds to the actual election taking place.


## Logstash+Kibana

__Important__: Backup all of your dashboards and visualizations before the upgrade.

It is more likely that older Kibana and Logstash will work with newer Elasticsearch, so we update
those EC2 instances last. Considering the fact that those instances are stateless, here is all that
needs to be done from AWS console (or aws-cli if you wish):

* teminate one kibana+logstash EC2 instance and wait for it to go through the "running ->
  shutting-down -> terminated" state transitions.
* wait for new one to be created and fully initialized. Best way to check that it's ready is to
  actually check the logs in Kibana itself.

__Pitfalls__ - Version mismatch:

While writing this guide, I first tried to upgrade Logstash+Kibana nodes before Elasticsearch ones
and sure enough I got this error:

> This version of Kibana requires Elasticsearch v6.2.2 on all nodes.

So all I could do at that point in order to bring Kibana back to life was to complete the upgrade on
the whole stack.

__Pitfalls__ - New index structure.

After Kibana upgrade to a new major version, there is a chance of index restructure:

> Your Kibana index is out of date, reset it or use the X-Pack upgrade assistant.

You did backup all of your dashboards, so just go ahead delete the `.kibana` index and re-import all
of previously saved dashboards and visualizations through management UI:

```
$ curl -s -XDELETE https://user:pass@elasticsearch.example.com:9201/.kibana
{"acknowledged":true}
```

Alternatively, there is a [manual upgrade
guide](https://www.elastic.co/guide/en/kibana/6.0/migrating-6.0-index.html) if you can't re-import
all of your dashboards.

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
or completely shut it shutdown.


Data nodes can be added safely at runtime, which is simply a matter of increasing the value of
`elasticsearch_data_node_count`. It is important to understand though, that this operation is better
done during the time of a lowest load on the cluster, since adding a data node will trigger shard
rebalancing and will incur a lot of network and EBS volume IO.
