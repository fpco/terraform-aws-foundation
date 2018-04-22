#!/bin/bash

set -o nounset
set -o pipefail
set -o errexit

empty_line() { 
    if [ -t 1 ]; then
       printf '\n'
    fi
}

invalid_code_found() {
    empty_line
    logger --priority 'user.err' \
           --tag "$(basename "$0")" \
           --stderr 'Invalid code found. Review the logs for offending entries.'
    exit 1
}

tf_should_not_have() {
    rg --type-add 'terraform:*.tf' \
       --type 'terraform' \
       "$@" \
        && invalid_code_found
}

declare -a inline_values=(
    "<<"
    "inline = "
)

for error in "${inline_values[@]}"
do
    tf_should_not_have "${error}"
done
