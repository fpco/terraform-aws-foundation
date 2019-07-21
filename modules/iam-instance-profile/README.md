# IAM Instance Profile

This module abstracts the useage pattern of IAM instance profile. The caller provides role/policy, and gets profile id to assign to instance.

Sample usgae:

```
module "iam_instance_profile" {
  source             = "../iam-instance-profile"
  assume_role_policy = "${data.aws_iam_policy_document.attach_ebs.json}"
  policy             = "${data.aws_iam_policy_document.attach_ebs_policy.json}"
  name_prefix        = "persistent-ebs"
}

module "server" {
  source             = "../asg"
  iam_profile = "${module.iam_instance_profile.iam_profile_id}"

  # other things here is ignored
}
```
