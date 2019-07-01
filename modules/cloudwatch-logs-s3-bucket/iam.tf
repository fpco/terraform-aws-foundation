data "aws_iam_policy_document" "cloudwatch-logs-writer" {
  statement {
    effect    = "Allow"
    resources = ["*"]

    actions = [
      "logs:putLogEvents",
      "logs:CreateLogStream",
      "logs:CreateLogGroup",
      "logs:DescribeLogStreams",
    ]
  }
}

data "aws_iam_policy_document" "cloudwatch-logs-writer-trust" {
  statement {
    effect  = "Allow"
    actions = ["sts:AssumeRole"]

    principals {
      type        = "Service"
      identifiers = ["ec2.amazonaws.com"]
    }
  }
}

resource "aws_iam_role" "cloudwatch-logs-writer" {
  name               = "cloudwatch_logs_writer"
  path               = "/"
  assume_role_policy = data.aws_iam_policy_document.cloudwatch-logs-writer-trust.json
}

resource "aws_iam_role_policy" "cloudwatch-logs-writer" {
  name   = "cloudwach_logs_writer_policy"
  role   = aws_iam_role.cloudwatch-logs-writer.id
  policy = data.aws_iam_policy_document.cloudwatch-logs-writer.json
}

resource "aws_iam_instance_profile" "cloudwatch-logs-writer" {
  name = "cloudwatch_logs_writer"
  role = aws_iam_role.cloudwatch-logs-writer.name
}

