#!/usr/bin/env bash

MAX_SLEEP=120
TOTAL_SLEEP=0
SLEEP_INTERVAL=3

show_help() {
  cat <<EOF
# $(basename "$0"): Create and revoke grant for a role required for credstash to work.

Besides IAM reader/writer policy attached to the role, credstash requires a KMS
grant to be created for the role. This script is here to help with creating and
revoking a grant during role creation. Due to the fact that roles are eventually
consistent in AWS, it might take a while for a role to become available, which
might cause a short waiting period (maximum of $MAX_SLEEP seconds).

Usage:

    $(basename "$0") \\
        (create (reader|writer) [--context CONTEXT]|revoke) \\
        kms-key-arn \\
        role-arn

EOF
}

invalid_args (){
  echo "$(basename "$0"): Invalid argument: $1" >&2
  echo "Run '$(basename "$0") --help' for usage information"
  exit 1
}


MODE=""
OPERATIONS_ARGS=""
case "$1" in
  --help)
    show_help
    exit 0
    ;;
  create)
    MODE="create"
    case "$2" in
      reader)
        OPERATIONS_ARGS="--operations Decrypt"
        ;;
      writer)
        OPERATIONS_ARGS="--operations GenerateDataKey"
        ;;
      *)
        invalid_args
    esac
    shift 2
    case "$1" in
      --context=*)
        CONTEXT="${1#--context=}"
        shift
        ;;
      --context)
        CONTEXT="$2"
        shift 2
        ;;
    esac
    ;;
  revoke)
    MODE="revoke"
    shift
    ;;
  *)
    invalid_args
    ;;
esac

KMS_KEY_ID="${1}"
GRANTEE="${2}"

if [ -z "${KMS_KEY_ID}" ] || [ -z "${GRANTEE}" ]; then
  invalid_args
fi

GRANTEE_NAME=$(echo "${2}" | awk -F ':' '{print $6}' | awk -F '/' '{print $2}')

if [ "$MODE" = "create" ]; then
  if [ -n "$CONTEXT" ]; then
    CONSTRAINTS_ARGS="--constraints=EncryptionContextEquals={$CONTEXT}"
  fi
  GRANT_CMD="aws kms create-grant --key-id $KMS_KEY_ID --grantee-principal $GRANTEE $CONSTRAINTS_ARGS $OPERATIONS_ARGS --name credstash:$GRANTEE_NAME"
  echo "Waiting for role ${GRANTEE_NAME} to become available."
  RESULT=$($GRANT_CMD 2>&1)
  while [ $? -ne 0 ]; do
    if [ "$TOTAL_SLEEP" -gt "$MAX_SLEEP" ]; then
      echo "Was unable to find the role within limit time of $MAX_SLEEP seconds:"
      echo "${RESULT}"
      exit 1
    fi
    echo "Still waiting for the role to become available: $TOTAL_SLEEP/$MAX_SLEEP ..."
    sleep $SLEEP_INTERVAL
    TOTAL_SLEEP=$((TOTAL_SLEEP + SLEEP_INTERVAL))
    RESULT=$($GRANT_CMD 2>&1)
  done
  echo "Role ${GRANTEE_NAME} is now available and grant was created for KMS Key."
  echo "${RESULT}"
elif [ "$MODE" = "revoke" ]; then
  GRANT_ID=$(aws kms list-grants --key-id $KMS_KEY_ID | \
                jq -r --arg grantee "${GRANTEE}" \
                   '.Grants[] | select(.GranteePrincipal == $grantee) | .GrantId')
  aws kms revoke-grant --key-id $KMS_KEY_ID --grant-id $GRANT_ID
else
  invalid_args
fi
