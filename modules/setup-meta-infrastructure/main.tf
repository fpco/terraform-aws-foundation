/**
 * ## MFA and IAM
 *
 * This module provides a "boxed" set of IAM groups and policies suitable
 * for managing account access through IAM and leveraging MFA.
 *
 * NOTE: need to document each group, policy, and the resulting permissions
 *
 */

#----------------------------------------------------------------------
# Data sources
#----------------------------------------------------------------------

data "aws_caller_identity" "current" {
}

# see the other .tf files for all the details..
