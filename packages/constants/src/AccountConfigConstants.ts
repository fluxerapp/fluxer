// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

const HOUR_MS = 60 * 60 * 1000;

export const InteractionTierModes = {
	LEGACY: 'legacy',
	ENFORCED: 'enforced',
} as const;

export type InteractionTierMode = ValueOf<typeof InteractionTierModes>;

export const GuildJoinGates = {
	OFF: 'off',
	VISITS_CAPABLE: 'visits_capable',
	ALWAYS: 'always',
} as const;

export type GuildJoinGate = ValueOf<typeof GuildJoinGates>;

export const DateOfBirthPolicies = {
	OFF: 'off',
	ASK: 'ask',
	REQUIRE_NEW_ACCOUNTS: 'require_new_accounts',
	REQUIRE_ALL: 'require_all',
} as const;

export type DateOfBirthPolicy = ValueOf<typeof DateOfBirthPolicies>;

export const LegacyDateOfBirthEnforcements = {
	OFF: 'off',
	NEW_ACCOUNTS: 'new_accounts',
	ALL: 'all',
} as const;

export type LegacyDateOfBirthEnforcement = ValueOf<typeof LegacyDateOfBirthEnforcements>;

export function dateOfBirthPolicyFromLegacy(params: {
	collect: boolean;
	enforcement: LegacyDateOfBirthEnforcement;
}): DateOfBirthPolicy {
	if (!params.collect) return DateOfBirthPolicies.OFF;
	switch (params.enforcement) {
		case LegacyDateOfBirthEnforcements.OFF:
			return DateOfBirthPolicies.ASK;
		case LegacyDateOfBirthEnforcements.NEW_ACCOUNTS:
			return DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS;
		case LegacyDateOfBirthEnforcements.ALL:
			return DateOfBirthPolicies.REQUIRE_ALL;
	}
}

export function collectsDateOfBirth(policy: DateOfBirthPolicy): boolean {
	return policy !== DateOfBirthPolicies.OFF;
}

export const IdentifierFirstModes = {
	AUTO: 'auto',
	ON: 'on',
	OFF: 'off',
} as const;

export type IdentifierFirstMode = ValueOf<typeof IdentifierFirstModes>;

export const VerificationBases = {
	AUTO: 'auto',
	CONFIRMED_EMAIL: 'confirmed_email',
	LOCAL_CREDENTIAL: 'local_credential',
} as const;

export type VerificationBasis = ValueOf<typeof VerificationBases>;

export type EffectiveVerificationBasis = Exclude<VerificationBasis, typeof VerificationBases.AUTO>;

export const EmailDeliveryModes = {
	OFF: 'off',
	SMTP: 'smtp',
} as const;

export type EmailDeliveryMode = ValueOf<typeof EmailDeliveryModes>;

export function emailDeliveryFromLegacy(params: {enabled: boolean; provider: 'smtp' | 'none'}): EmailDeliveryMode {
	return params.enabled && params.provider === 'smtp' ? EmailDeliveryModes.SMTP : EmailDeliveryModes.OFF;
}

export const EmailDeliveryStates = {
	ON: 'on',
	OFF_USERNAME_MODE: 'off_username_mode',
	OFF_DISABLED: 'off_disabled',
	OFF_INCOMPLETE: 'off_incomplete',
} as const;

export type EmailDeliveryState = ValueOf<typeof EmailDeliveryStates>;

export const EmailCodeAvailability = {
	AVAILABLE: 'available',
	IDENTITY_MODE_USERNAME: 'identity_mode_username',
	DELIVERY_OFF: 'delivery_off',
	NO_OWNED_EMAIL: 'no_owned_email',
} as const;

export type EmailCodeAvailability = ValueOf<typeof EmailCodeAvailability>;

export const InstantAccountAvailability = {
	AVAILABLE: 'available',
	DISABLED: 'disabled',
	CAPTCHA_OFF: 'captcha_off',
	REGISTRATION_CLOSED: 'registration_closed',
	APPROVAL_REQUIRED: 'approval_required',
} as const;

export type InstantAccountAvailability = ValueOf<typeof InstantAccountAvailability>;

export const RegistrationModes = {
	OPEN: 'open',
	APPROVAL: 'approval',
	CLOSED: 'closed',
} as const;

export type RegistrationMode = ValueOf<typeof RegistrationModes>;

export const CleanupModes = {
	OFF: 'off',
	AFTER_INACTIVITY: 'after_inactivity',
} as const;

export const LastSessionCleanupModes = {
	OFF: 'off',
	AFTER_GRACE: 'after_grace',
} as const;

export const ClaimAccountNextSteps = {
	NONE: 'none',
	CONFIRM_EMAIL: 'confirm_email',
} as const;

export const AccountCleanupDefaults = {
	SCHEDULED_DELETION_GRACE_MS: 24 * HOUR_MS,
	MIN_ACCOUNT_AGE_MS: 24 * HOUR_MS,
	UNCLAIMED_INACTIVE_DAYS: 90,
	DOB_MISSING_INACTIVE_DAYS: 7,
	INACTIVITY_DAYS: 730,
	INACTIVITY_WARNING_LEAD_DAYS: 30,
	MIN_INACTIVE_DAYS: 1,
	MAX_INACTIVE_DAYS: 3650,
} as const;

export const InstantAccountLimits = {
	MIN_POW_ESCALATION_PER_HOUR: 10,
	MAX_POW_ESCALATION_PER_HOUR: 1_000_000,
	DEFAULT_POW_ESCALATION_PER_HOUR: 2000,
} as const;
