variable "create_groups" {
  description = "Set to 0 to disable creating the 'admin', 'power-user', and 'setup-mfa' groups.  This should be done for accounts that users do not sign into directly (only delegated access)."
  default     = 1
  type        = string
}

variable "groups_require_assume_role" {
  description = "Set to 1 to give the 'admin' and 'power-user' groups no privileges aside from being able to assume roles.  This is a best practice and recommended for all new accounts.  Only relevant if create_groups = 1."
  default     = 0
  type        = string
}

variable "trust_account_ids" {
  description = "A list of accounts that are trusted.  Trusted accounts can assume the 'admin' and 'power-user' roles in the current account."
  default     = []
  type        = list(string)
}

variable "admin_control_account_ids" {
  description = "List of accounts that users in the 'admin' group can assume the 'admin' and 'power-user' roles in.  These accounts must trust the current account.  Only relevant if create_groups = 1."
  default     = []
  type        = list(string)
}

variable "power_user_control_account_ids" {
  description = "List of accounts that users in the 'power-user' group can assume the 'power-user' role in.  These accounts must trust the current account.  Only relevant if create_groups = 1."
  default     = []
  type        = list(string)
}

variable "admin_group_name" {
  description = "The name of the administrators group.  A common value for new accounts is 'super-admin', since these admins have control over other accounts.  Only relevant if create_groups = 1."
  default     = "admin"
  type        = string
}

variable "power_user_group_name" {
  description = "The name of the power users group.  A common value for new accounts with delegation is 'super-power-user', since these admins have control over other accounts.  Only relevant if create_groups = 1."
  default     = "power-user"
  type        = string
}

variable "admin_group_members" {
  description = "Users in the 'admin' group.  Only relevant if create_groups = 1."
  default     = []
  type        = list(string)
}

variable "power_user_group_members" {
  description = "Users in the 'power-user' group.  Only relevant if create_groups = 1."
  default     = []
  type        = list(string)
}

variable "setup_mfa_group_members" {
  description = "Users in the 'setup-mfa' group.  Only relevant if create_groups = 1."
  default     = []
  type        = list(string)
}

variable "set_password_policy" {
  description = "Set to 0 to disable setting the account password policy"
  default     = 1
  type        = string
}

variable "min_password_length" {
  description = "Minimum length to require for user passwords"
  default     = 12
  type        = number
}

variable "max_password_age" {
  description = "The number of days that an user password is valid"
  default     = 90
  type        = number
}
