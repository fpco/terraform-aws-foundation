{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "NotAction": "iam:*",
            "Resource": "*",
            "Condition": {
                "Bool": {
                    "aws:MultiFactorAuthPresent": "true"
                }
            }
        },
        {
            "Effect": "Allow",
            "Action": [
                "iam:*LoginProfile",
                "iam:*AccessKey*"
            ],
            "Resource": "arn:aws:iam::${account_id}:user/$${aws:username}",
            "Condition": {
                "Bool": {
                    "aws:MultiFactorAuthPresent": "true"
                }
            }
        },
        {
            "Effect": "Allow",
            "Action": [
                "iam:ListAccount*",
                "iam:GetAccount*",
                "iam:ListUsers",
                "iam:ListRoles"
            ],
            "Resource": "*",
            "Condition": {
                "Bool": {
                    "aws:MultiFactorAuthPresent": "true"
                }
            }
        },
        {
            "Effect": "Allow",
            "Action": [
                "iam:ListMFADevices"
            ],
            "Resource": "*"
        }
    ]
}