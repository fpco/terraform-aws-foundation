aws acm import-certificate --certificate file://gitlab.pem --private-key file://gitlab-key.pem --certificate-chain file://ca.pem --region eu-central-1  > cert.json
