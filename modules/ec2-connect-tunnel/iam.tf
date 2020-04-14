# allows connecting with SSM manager
resource "aws_iam_role_policy_attachment" "ssm_instance" {
  role       = module.asg.asg_iam_role_name
  policy_arn = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
}
