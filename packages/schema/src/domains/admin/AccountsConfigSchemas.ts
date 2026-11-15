// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	AccountCleanupDefaults,
	InstantAccountLimits,
	LegacyDateOfBirthEnforcements,
} from '@fluxer/constants/src/AccountConfigConstants';
import {
	CleanupModeSchema,
	DateOfBirthPolicySchema,
	GuildJoinGateSchema,
	IdentifierFirstModeSchema,
	InteractionTierModeSchema,
	LastSessionCleanupModeSchema,
	VerificationBasisSchema,
} from '@fluxer/schema/src/domains/account/AccountTierSchemas';
import {z} from 'zod';

const LegacyDateOfBirthEnforcementSchema = z
	.enum([
		LegacyDateOfBirthEnforcements.OFF,
		LegacyDateOfBirthEnforcements.NEW_ACCOUNTS,
		LegacyDateOfBirthEnforcements.ALL,
	])
	.describe('Deprecated, use date_of_birth');

const InstantAccountsConfigSchema = z.object({
	enabled: z.boolean().describe('Whether accounts can be created from just a display name. Needs the captcha to be on'),
	pow_escalation_per_hour: z
		.number()
		.int()
		.min(InstantAccountLimits.MIN_POW_ESCALATION_PER_HOUR)
		.max(InstantAccountLimits.MAX_POW_ESCALATION_PER_HOUR)
		.describe('Every this many accounts created from a display name in the current hour doubles the captcha cost'),
});

export const AuthConfigSchema = z.object({
	identifier_first: IdentifierFirstModeSchema,
	instant_accounts: InstantAccountsConfigSchema,
});

export type AuthConfig = z.infer<typeof AuthConfigSchema>;

const InactiveDaysSchema = z
	.number()
	.int()
	.min(AccountCleanupDefaults.MIN_INACTIVE_DAYS)
	.max(AccountCleanupDefaults.MAX_INACTIVE_DAYS);

const UnclaimedAccountCleanupSchema = z.object({
	mode: CleanupModeSchema.describe('Whether accounts that can never be signed in to again are deleted'),
	inactive_days: InactiveDaysSchema.describe('Days without use before such an account is deleted'),
	on_last_session_end: LastSessionCleanupModeSchema,
});

const DateOfBirthMissingCleanupSchema = z.object({
	mode: CleanupModeSchema.describe(
		'Whether accounts that were created without a date of birth and never entered one are deleted',
	),
	inactive_days: InactiveDaysSchema.describe('Days without use before such an account is deleted'),
});

const InactivityDeletionSchema = z.object({
	mode: CleanupModeSchema.describe(
		'Whether accounts unused for a long time are deleted. An account is only deleted after a warning email reached it',
	),
	inactive_days: InactiveDaysSchema.describe('Days without use before the warning email is sent'),
	warning_lead_days: z.number().int().min(1).max(365).describe('Days between the warning email and the deletion'),
});

export const AccountsConfigSchema = z.object({
	date_of_birth: DateOfBirthPolicySchema,
	interaction_tier: InteractionTierModeSchema,
	verification_basis: VerificationBasisSchema,
	guild_join_gate: GuildJoinGateSchema,
	unclaimed_account_cleanup: UnclaimedAccountCleanupSchema,
	dob_missing_cleanup: DateOfBirthMissingCleanupSchema,
	inactivity_deletion: InactivityDeletionSchema,
});

export type AccountsConfig = z.infer<typeof AccountsConfigSchema>;

export const AuthConfigUpdateRequest = z.object({
	identifier_first: z
		.union([IdentifierFirstModeSchema, z.boolean().describe('Deprecated: true means on and false means off')])
		.optional(),
	instant_accounts: InstantAccountsConfigSchema.partial().optional(),
});

export type AuthConfigUpdateRequest = z.infer<typeof AuthConfigUpdateRequest>;

export const AccountsConfigUpdateRequest = z.object({
	date_of_birth: DateOfBirthPolicySchema.optional(),
	interaction_tier: InteractionTierModeSchema.optional(),
	verification_basis: VerificationBasisSchema.optional(),
	guild_join_gate: GuildJoinGateSchema.optional(),
	unclaimed_account_cleanup: UnclaimedAccountCleanupSchema.partial().optional(),
	dob_missing_cleanup: DateOfBirthMissingCleanupSchema.partial().optional(),
	inactivity_deletion: InactivityDeletionSchema.partial().optional(),
	date_of_birth_enforcement: LegacyDateOfBirthEnforcementSchema.optional(),
	abandoned_account_cleanup: z
		.boolean()
		.optional()
		.describe('Deprecated, use unclaimed_account_cleanup and dob_missing_cleanup'),
});

export type AccountsConfigUpdateRequest = z.infer<typeof AccountsConfigUpdateRequest>;
