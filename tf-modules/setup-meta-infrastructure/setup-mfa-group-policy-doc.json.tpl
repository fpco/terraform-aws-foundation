{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "AllowUsersToCreateDeleteTheirOwnVirtualMFADevices",
            "Effect": "Allow",
            "Action": "iam:*VirtualMFADevice",
            "Resource": "arn:aws:iam::${account_id}:mfa/$${aws:username}"
        },
        {
            "Sid": "AllowUsersToEnableSyncDisableTheirOwnMFADevices",
            "Effect": "Allow",
            "Action": [
                "iam:DeactivateMFADevice",
                "iam:EnableMFADevice",
                "iam:ListMFADevices",
                "iam:ResyncMFADevice"
            ],
            "Resource": "arn:aws:iam::${account_id}:user/$${aws:username}"
        },
        {
            "Sid": "AllowUsersToListVirtualMFADevices",
            "Effect": "Allow",
            "Action": "iam:ListVirtualMFADevices",
            "Resource": "arn:aws:iam::${account_id}:mfa/*"
        },
        {
            "Sid": "AllowUsersToListUsersInConsole",
            "Effect": "Allow",
            "Action": "iam:ListUsers",
            "Resource": "arn:aws:iam::${account_id}:user/*"
        }
    ]
}
