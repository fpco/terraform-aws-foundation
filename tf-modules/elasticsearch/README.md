# Elasticsearch

## Check list for nodes configuration.

1. Set JVM heap size to 50% of RAM, but no more than 26Gb.
2. There is no swap on AWS instances by default, no need to lock it.
3. `ulimit -n 65536` is set by `deb` package:
   * Checked using [_nodes/stats/process?filter_path=**.max_file_descriptors]
   * Although:
     ```bash
     $ sudo su elasticsearch
     $ ulimit -n
     1024
     ```

4. Virtual memory: `sysctl -w vm.max_map_count=262144` is set by `deb` package.
5. Number of threads:

    ```bash
    $ sudo su elasticsearch
    $ ulimit -u
    7948
    ```

## Data volumes

Because of
terraform's [issue](https://github.com/hashicorp/terraform/issues/2957) with EBS
volumes, plus inability to remount the same volume with another instance
automatically we have to mount it manually using `awscli` on the instance
itself. Due to this workaround __internet access is required__ for __all__
elasticsearch nodes. Remounting technique is similar to the ones described here:

  * https://github.com/hashicorp/terraform/issues/2957#issuecomment-150613677
  * http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-using-volumes.html


## Dependencies
Another reason for internet access is installation of
dependencies upon initialization. Despite that dependencies can be burned into
an AMI image and there is already a Packer config for this in `scripts` folder,
EBS volume mounting needs access to
[AWS endpoint](http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-endpoints.html).

## Networking

Due to the points described in above two sections, and because Elasticsearch
cluster is deployed in private subnets, NAT gateway(s) or some other means for
internet access has to be provided to the cluster.

