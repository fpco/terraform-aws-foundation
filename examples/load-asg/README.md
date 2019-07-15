# AWS Load-based Autoscaling w/ Terraform 

## Overview

### Example Description

This is an example of creating load-balancing policies that work with Cloudwatch monitoring to dynamically scale an autoscaling group. We use a trivial static app which shows the local IP of the instance. In this way you can visually see instances being created. The stress on the web application is simulated using `stress-ng` which is automatically installed on each instance.

### Requirements
- Terraform 0.11.7
- A recent version of `jl` which can be downloaded from [here](https://github.com/chrisdone/jl/releases). Be sure to put the binary somewhere on your path (eg in `/usr/local/bin`) and name it `jl`.
* `envsubst` which is part of `gettext`. This already installed on most Linux systems, but if not, install the appropriate system package (`gettext` on most systems, including brew on Mac, but `gettext-base` on Debian/Ubuntu).

## Deploying the Example Infrastructure

Make sure that `variables.tf` are to your liking. In particular check `name` and `region`. Then run some make targets:

```bash
ᐅ make generate-ssh-key
ᐅ make network
ᐅ make plan
ᐅ make apply
ᐅ make render-ssh-config
```

## Running Load Tests

### Overview

The example builds an auto-scaling group with a minimum of 2 and a maximum of 10 nodes. It creates the load balancing policies and Cloudwatch alarms necessary to see the impact of load stress. The [`stress-ng` tool][1] is automatically installed on each instance and its used to simulate simulate various types of load events (CPU overusage, memory full, etc).

For each test you run you can follow the load by refreshing the Cloudwatch Metric Summary in the AWS portal. When load rises above the threshold set, within the time set, an associated Cloudwatch `up` alarm is generated, as you will see in the portal. This alarm is connected to the auto-scaling group and causes it to create new instances.

When the stress test ends and usage returns back to normal, a Cloudwatch `down` alarm is generated, which you will also see in the portal. This alarm is also connected to the auto-scaling group and causes it to destroy some of the existing instances.

If you refresh the ELB FQDN link in your browser (one of the outputs provided by Terraform) you can see the IP addresses for the instances running, as the ELB alternates sendings requests to the various existing instances. As new instances are created, new IPs will show up. As instances are terminated, there will be fewer different IP addresses showing up.

Note that more instances are created on the `up` alarm than eliminated in the `down` alarm. This is a best practice, since usually when there is stress, it lasts for a while. You will notice that `down` alarms are created over and over again, which is as it should be as long as the the stress level is below the dangerous threshold. Eventually when the stress is finally gone, you will get back down to the minimum of two instances, but never below that. Subsequent `down` alarms are ignored when the instance level is at the minimum.

Please note that in the current version we only render ssh for two instances. As a consequence, if you rerun the load tests it will not go above the threshold, since only two instances are being stressed, but the average is over 4 instances. Hence, in the current version you will not see more than 4 instances, even though 10 is the theoretical maximum. This will be improved in subsequent versions. For this reason as well, you should run each stress test after the autoscale group is back down to two instances.

In future versions we will add the following capabilities:

- Add memory and disk metrics (the memory metric is already in the code, but because this requires installing a [new cloudwatch agent][2] activating it is outside the scope of this first version)
- deal with interactions between metrics (i.e. deal with scaling in situations where e.g. memory still being stressed even if cpu usage has gone down)
- build a test infrastructure that runs stress on `n` instances (not the hard-coded `n=2` of current version)
- add examples of other metrics
- give the tester more control over the various property "knobs"

### CPU Load Testing

Run the following command:

```bash
ᐅ make generate-cpu-stress
```

This will render two ssh instances and run a cpu stress test on each of them.


[1]: https://www.tecmint.com/linux-cpu-load-stress-test-with-stress-ng-tool/
[2]: https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/mon-scripts.html
