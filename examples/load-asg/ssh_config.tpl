Host asg-server-1-sysadmin
    user ubuntu
    hostname ${SERVER_IP_1}
Host asg-server-2-sysadmin
    user ubuntu
    hostname ${SERVER_IP_2}
Host *
    identityfile ${PWD}/id_rsa
    IdentitiesOnly yes
    StrictHostKeyChecking accept-new
