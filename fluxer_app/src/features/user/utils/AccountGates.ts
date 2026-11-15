// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {GuildJoinGates} from '@fluxer/constants/src/AccountConfigConstants';
import {
	type AccountTier,
	AccountTiers,
	type AccountVerificationNeed,
	AccountVerificationNeeds,
	TierExemptions,
	type TierGatedAction,
} from '@fluxer/constants/src/AccountTierConstants';
import {evaluateTierGate, resolveVerificationNeed, type TierGateDenial} from '@fluxer/constants/src/AccountTierRules';

export interface AccountGateUser {
	readonly accountTier: AccountTier;
	readonly verificationNeed: AccountVerificationNeed | null;
	readonly email: string | null | undefined;
}

export interface AccountGateContext {
	readonly isGuildOwner?: boolean;
}

interface AccountGateAllowed {
	readonly allowed: true;
	readonly action: TierGatedAction;
	readonly tier: AccountTier;
	readonly need: AccountVerificationNeed | null;
	readonly errorCode: null;
}

export type AccountGateResult = AccountGateAllowed | TierGateDenial;

export function canPerform(
	user: AccountGateUser | null | undefined,
	action: TierGatedAction,
	ctx: AccountGateContext = {},
): AccountGateResult {
	const tier = user?.accountTier ?? AccountTiers.UNCLAIMED;
	const hasEmail = user?.email != null;
	const result = evaluateTierGate({
		tier,
		mode: RuntimeConfig.interactionTier,
		action,
		identityMode: RuntimeConfig.accountIdentity,
		hasEmail,
		guildJoinGateApplies: RuntimeConfig.guildJoinGate !== GuildJoinGates.OFF,
		exemptions: ctx.isGuildOwner ? [TierExemptions.GUILD_OWNER] : [],
	});
	if (!result.allowed) return result;
	const need = user
		? user.verificationNeed
		: resolveVerificationNeed({tier, identityMode: RuntimeConfig.accountIdentity, hasEmail});
	return {allowed: true, action, tier, need, errorCode: null};
}

export function needsEmailConfirmation(user: Pick<AccountGateUser, 'verificationNeed'> | null | undefined): boolean {
	return RuntimeConfig.emailsEnabled && user?.verificationNeed === AccountVerificationNeeds.CONFIRM_EMAIL;
}
