Host ${GITLAB_NAME}.${DNS_ZONE_NAME}
    identityfile ${PWD}/id_rsa
    user git
Host data-ops-gitlab
    identityfile ${PWD}/id_rsa
    user ubuntu
    hostname ${SERVER_IP}
