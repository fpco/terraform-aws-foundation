provider "aws" {
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
}
resource "aws_ebs_volume" "main" {
    availability_zone = "${var.az}"
    size = "${var.ebs_volume_size}"
    type = "${var.ebs_volume_type}"
#   iops = "${var.ebs_volume_iops}"
    encrypted = "${var.ebs_volume_encrypted}"
    kms_key_id = "${var.ebs_volume_kms_key_id}"
    snapshot_id = "${var.ebs_volume_snapshot_id}"
    tags {
        Name = "${var.name}"
    }
}
module "single_instance_asg" {
    source = "../asg"
    ami = "${var.ami}"
    iam_profile = "${aws_iam_instance_profile.attach_ebs.id}"
    az_list = "${var.az}"
    desired_capacity = 1
    elb_names = "${var.load_balancers}"
    instance_type = "${var.instance_type}"
    max_nodes = 1
    min_nodes = 1
    key_name = "${var.key_name}"
    name = "${var.name}"
    suffix = "${var.suffix}"
    region = "${var.region}"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    subnet_ids = "${var.subnet_id}"
    security_group_ids = "${var.security_group_ids}"
    root_volume_type = "${var.root_volume_type}"
    root_volume_size = "${var.root_volume_size}"
    #user_data = "${module.instance-init.user_data}"
    user_data = <<END_INIT
#!/bin/bash
${var.initial_init}
export AWS_DEFAULT_REGION=${var.region}
VOLUME_ID=${aws_ebs_volume.main.id}
INSTANCE_ID=$$(ec2metadata --instance-id)
echo "will attach $$VOLUME_ID via the AWS API"
aws ec2 attach-volume --volume-id $VOLUME_ID --instance-id $INSTANCE_ID --device /dev/xvdf
while [ $$? -ne 0 ]; do
  sleep 5
  ls /dev/xvdf
done
${var.extra_init}
END_INIT
}
# provisioning for instance
#module "instance-init" {
#    source = "../tf-modules/consul-agent-generic-init"
#    region = "${var.region}"
#    service = "${var.role_name}"
#    extra_pillar = "${var.extra_pillar}"
#    extra_init = "${var.extra_user_data}"
#    extra_init = <<EOF
#echo "customize this node's init.."
#date
#consul --version
#salt-call --version
#uname -a
#df -h
#EOF
#}
