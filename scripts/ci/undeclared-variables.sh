#!/bin/bash

set -o nounset

pushd "$(dirname "$(basename "${0}")")/modules" > /dev/null 

errors=$(mktemp)
echo '0' > "${errors}"

increase_errors() {
    count=$(cat "${errors}")
    count=$((count + 1))
    echo "${count}" > "${errors}"
}

emphasize() {
    NORMAL=$(tput sgr0)
    COLOUR=$(tput setaf 5)
    printf "%s:\n" "${COLOUR}${*}${NORMAL}"
}

# Needs a patched 'terraform-index' at github.com/kerscher/terraform-index
for folder in $(ls);
do
    pushd "${folder}" > /dev/null
    
    emphasize "$(pwd)"

    # shellcheck disable=SC2035
    module_index=$(terraform-index *.tf)
    [ ! "${?}" == 1 ] && increase_errors
    
    echo "${module_index}" | jq ".Variables | map(select(.Type == \"undeclared\")) | map({Name: .Name, Module: \"${folder}\", Filename: (.Location | .Filename)}) | .[]"
    popd > /dev/null
done

popd > /dev/null

printf "\nNumber of errors: %s" "$(cat "${errors}")"

[ "$(cat "${errors}")" -gt 0 ] && exit 1
