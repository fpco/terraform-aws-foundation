# Building an Ops Stack

This project aims to be..

* an example, a reference stack
* an extensible framework for the ops developer under (multiple dimensions of) pressure
* a highly-scalable and flexible implementation of modern distributed systems architecture and paradigms
* focused on security, maintainability, repeatability, reliability, and simple but powerful workflows
* a way for ops to deal with the ever-evolving nature of the ops-landscape, while also continuing to support the legacy apps of yesteryear


## The Right Tool for the Relevant Problems

* [Packer](https://packer.io) - creates pre-baked VM images for nodes in our network
* [Terraform](https://terraform.io) - orchestrates resources in the cloud, declarative expression at the core of ops (network, node, cluster, service)
* [Saltstack](https://saltstack.com), [saltstack-formulas](https://github.com/saltstack-formulas/), [fpco-salt-formula](https://github.com/fpco/fpco-salt-formula), [bootstrap-salt-formula](https://github.com/fpco/bootstrap-salt-formula) - Simple and sane _Configuration Management_, fault-tolerant (masterless) highly-expressive, and highly-scalable, with optional _Remote Execution_ to boot.
* [Amazon Auto-Scaling Groups](http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/WhatIsAutoScaling.html) - Robust platforms must be self-healing.
* [Docker](https://docker.com) - Most services/applications run on the platform will do so as docker containers (but not all, legacy apps are easily supported). 
* [Consul](https://consul.io), [consul-template](https://github.com/hashicorp/consul-template), [consulkv](https://github.com/spiritloose/consulkv) - For service discovery, distributed key/value store, and simplifed automation and orchestration in the distributed system


## Next Steps

* Interested in how everything is put together? See the [system architecture](arch.md).
* Familiar with Terraform, AWS, and/or Saltstack, and interested in making use of the stack? There is a growing list of [tutorials](how-to/index.md).
* Developers looking to hack on the underlying system should consult [how to extend](extend.md).
* If you have been asked to managed a system built on this platform, there is the [administrator's guide](admin/index.md).
* What happens when...? [FAQ](faq.md)
