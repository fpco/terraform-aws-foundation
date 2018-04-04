aws acm import-certificate --certificate file://gitlab.pem --private-key file://gitlab-key.pem --certificate-chain file://ca.pem --region ${REGION}  > upload-gen-cert.json
