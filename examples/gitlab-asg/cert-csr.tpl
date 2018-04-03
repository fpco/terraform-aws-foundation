{
    "CN": "${GITLAB_URL}",
    "hosts": [
        "${GITLAB_URL}",
        "${REGISTRY_URL}"
    ],
    "key": {
        "algo": "rsa",
        "size": 2048
    },
    "names": [
        {
            "O": "${ORGANIZATION}",
            "C": "${COUNTRY}",
            "ST": "${STATE}",
            "L": "${LOCALITY}"
        }
    ]
}
