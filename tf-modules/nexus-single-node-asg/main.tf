/** 
 * Run a Gitlab on a single EC2 instance.
 * This instance will be part of a single-node autoscaling group
 * that shares an EBS volume to store data.
 * 
 * Note that there is a peculiarity with the EBS volume in that it
 * requires some manual setup the very first time to make it available
 * for use (unless a snapshot id is supplied):
 *
 * parted --script /dev/xvdf -- mklabel msdos
 * parted --script /dev/xvdf -- mkpart primary 0 -1
 * mkfs -t ext4 -F /dev/xvdf1
 * e2label /dev/xvdf1 nexus
 *
 */

module "snasg" {
  source                  = "../../tf-modules/single-node-asg"
  name                    = "test"
  az                      = "${var.az}"
  key_name                = "${var.key_name}"
  key_file                = "${var.key_file}"
  ami                     = "${var.instance_ami}"
  instance_type           = "${var.instance_type}"
  name_suffix             = "nexus-asg"
  subnet_id               = "${var.subnet_id}"
  security_group_ids      = "${var.security_group_ids}"
  region                  = "${var.region}"
  root_volume_type        = "${var.root_volume_type}"
  root_volume_size        = "${var.root_volume_size}"
  data_volume_type        = "${var.data_volume_type}"
  data_volume_size        = "${var.data_volume_size}"
  data_volume_encrypted   = "${var.data_volume_encrypted}"
  data_volume_kms_key_id  = "${var.data_volume_kms_key_id}"
  data_volume_snapshot_id = "${var.data_volume_snapshot_id}"
  data_volume_iops        = "${var.data_volume_iops}"

  init_prefix = <<END_INIT
apt-get update
${module.init-install-awscli.init_snippet}
${module.init-install-ops.init_snippet}
END_INIT

  init_suffix = <<END_INIT
mkdir -p /nexus
mount /dev/xvdf1 /nexus

cp /etc/fstab /etc/fstab.orig
echo "LABEL=nexus            /nexus  ext4   defaults,nofail     0 2" >> /etc/fstab

apt-get install -y docker docker.io
cat >/etc/rc.local <<EOL
#!/bin/bash
set -e
set -x
mkdir -p /nexus/data
chown -R 200 /nexus/data
docker run \
  --detach \
  --publish 8081:8081 \
  --restart always \
  --volume /nexus/data:/sonatype-work \
  sonatype/nexus
EOL
/etc/rc.local
END_INIT
}

module "init-install-awscli" {
  source = "../../tf-modules/init-snippet-install-awscli"
}

module "init-install-ops" {
  source = "../../tf-modules/init-snippet-install-ops"
}
