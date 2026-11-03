// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {userGuildVisitsActive} from '@app/api/experiment/GuildVisitsGate';
import {getCachedAccountIdentityMode} from '@app/api/instance/AccountIdentityModeCache';
import {envEmailDeliveryState} from '@app/api/instance/EmailDelivery';
import {
	AccountCleanupDefaults,
	CleanupModes,
	DateOfBirthPolicies,
	type DateOfBirthPolicy,
	type EffectiveVerificationBasis,
	type EmailDeliveryState,
	type GuildJoinGate,
	GuildJoinGates,
	IdentifierFirstModes,
	InstantAccountLimits,
	type InteractionTierMode,
	InteractionTierModes,
	LastSessionCleanupModes,
	VerificationBases,
} from '@fluxer/constants/src/AccountConfigConstants';
import {resolveEffectiveVerificationBasis} from '@fluxer/constants/src/AccountTierRules';
import type {AccountsConfig, AuthConfig} from '@fluxer/schema/src/domains/admin/AccountsConfigSchemas';

export interface AccountsConfigDefaults {
	auth: AuthConfig;
	accounts: AccountsConfig;
}

export function seededInactivityDays(): number {
	return Math.max(AccountCleanupDefaults.INACTIVITY_DAYS, Config.inactivityDeletionThresholdDays ?? 0);
}

function inactivityDeletionDefaults(): AccountsConfig['inactivity_deletion'] {
	return {
		mode: CleanupModes.AFTER_INACTIVITY,
		inactive_days: Math.min(seededInactivityDays(), AccountCleanupDefaults.MAX_INACTIVE_DAYS),
		warning_lead_days: AccountCleanupDefaults.INACTIVITY_WARNING_LEAD_DAYS,
	};
}

export function abandonedAccountCleanupDefaults(
	on: boolean,
): Pick<AccountsConfig, 'unclaimed_account_cleanup' | 'dob_missing_cleanup'> {
	return {
		unclaimed_account_cleanup: {
			mode: on ? CleanupModes.AFTER_INACTIVITY : CleanupModes.OFF,
			inactive_days: AccountCleanupDefaults.UNCLAIMED_INACTIVE_DAYS,
			on_last_session_end: on ? LastSessionCleanupModes.AFTER_GRACE : LastSessionCleanupModes.OFF,
		},
		dob_missing_cleanup: {
			mode: on ? CleanupModes.AFTER_INACTIVITY : CleanupModes.OFF,
			inactive_days: AccountCleanupDefaults.DOB_MISSING_INACTIVE_DAYS,
		},
	};
}

export function hostedAccountsDefaults(): AccountsConfigDefaults {
	return {
		auth: {
			identifier_first: IdentifierFirstModes.OFF,
			instant_accounts: {enabled: true, pow_escalation_per_hour: InstantAccountLimits.DEFAULT_POW_ESCALATION_PER_HOUR},
		},
		accounts: {
			date_of_birth: DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS,
			interaction_tier: InteractionTierModes.LEGACY,
			verification_basis: VerificationBases.AUTO,
			guild_join_gate: GuildJoinGates.ALWAYS,
			...abandonedAccountCleanupDefaults(false),
			inactivity_deletion: inactivityDeletionDefaults(),
		},
	};
}

export function selfHostedAccountsDefaults(params: {
	freshInstall: boolean;
	dateOfBirth?: DateOfBirthPolicy;
}): AccountsConfigDefaults {
	return {
		auth: {
			identifier_first: IdentifierFirstModes.AUTO,
			instant_accounts: {enabled: false, pow_escalation_per_hour: InstantAccountLimits.DEFAULT_POW_ESCALATION_PER_HOUR},
		},
		accounts: {
			date_of_birth: params.dateOfBirth ?? DateOfBirthPolicies.OFF,
			interaction_tier: params.freshInstall ? InteractionTierModes.ENFORCED : InteractionTierModes.LEGACY,
			verification_basis: VerificationBases.AUTO,
			guild_join_gate: GuildJoinGates.ALWAYS,
			...abandonedAccountCleanupDefaults(params.freshInstall),
			inactivity_deletion: inactivityDeletionDefaults(),
		},
	};
}

export function fallbackAccountsDefaults(): AccountsConfigDefaults {
	if (!Config.instance.selfHosted) return hostedAccountsDefaults();
	return selfHostedAccountsDefaults({freshInstall: false});
}

let cachedAccounts: AccountsConfig | null = null;
let cachedEmailDeliveryState: EmailDeliveryState | null = null;

export function getCachedAccountsConfig(): AccountsConfig {
	return cachedAccounts ?? fallbackAccountsDefaults().accounts;
}

export function setCachedAccountsConfig(config: AccountsConfig | null): void {
	cachedAccounts = config;
}

export function getEmailDeliveryState(): EmailDeliveryState {
	return cachedEmailDeliveryState ?? envEmailDeliveryState(getCachedAccountIdentityMode());
}

export function setCachedEmailDeliveryState(state: EmailDeliveryState | null): void {
	cachedEmailDeliveryState = state;
}

export function getInteractionTierMode(): InteractionTierMode {
	return getCachedAccountsConfig().interaction_tier;
}

function getGuildJoinGate(): GuildJoinGate {
	return getCachedAccountsConfig().guild_join_gate;
}

export function guildJoinGateApplies(params: {userId: UserID; visitsCapable: boolean}): boolean {
	const gate = getGuildJoinGate();
	if (gate === GuildJoinGates.OFF) return false;
	if (!userGuildVisitsActive(params.userId)) return false;
	return gate === GuildJoinGates.ALWAYS || params.visitsCapable;
}

export function getEffectiveVerificationBasis(): EffectiveVerificationBasis {
	return resolveEffectiveVerificationBasis({
		identityMode: getCachedAccountIdentityMode(),
		basis: getCachedAccountsConfig().verification_basis,
		deliveryState: getEmailDeliveryState(),
	});
}

export function getDateOfBirthPolicy(): DateOfBirthPolicy {
	return getCachedAccountsConfig().date_of_birth;
}
