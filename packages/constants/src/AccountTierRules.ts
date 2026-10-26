// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type EffectiveVerificationBasis,
	EmailCodeAvailability,
	type EmailDeliveryMode,
	EmailDeliveryModes,
	type EmailDeliveryState,
	EmailDeliveryStates,
	type IdentifierFirstMode,
	IdentifierFirstModes,
	InstantAccountAvailability,
	type InteractionTierMode,
	InteractionTierModes,
	type RegistrationMode,
	RegistrationModes,
	VerificationBases,
	type VerificationBasis,
} from '@fluxer/constants/src/AccountConfigConstants';
import {type AccountIdentityMode, AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {
	type AccountTier,
	AccountTiers,
	type AccountVerificationNeed,
	AccountVerificationNeeds,
	type LocalCredentialKind,
	LocalCredentialKinds,
	type PurchaseBlockedReason,
	PurchaseBlockedReasons,
	type RestrictedAccountTier,
	type TierDenialError,
	TierDenialErrors,
	type TierExemption,
	TierExemptions,
	type TierGatedAction,
	TierGatedActions,
} from '@fluxer/constants/src/AccountTierConstants';
import {type CaptchaProvider, CaptchaProviders} from '@fluxer/constants/src/CaptchaConstants';
import {hasUserTrait, type UserTraitSet, UserTraits} from '@fluxer/constants/src/UserTraitConstants';

export function resolveEffectiveVerificationBasis(params: {
	identityMode: AccountIdentityMode;
	basis: VerificationBasis;
	deliveryState: EmailDeliveryState;
}): EffectiveVerificationBasis {
	if (params.identityMode === AccountIdentityModes.USERNAME) return VerificationBases.LOCAL_CREDENTIAL;
	if (params.deliveryState !== EmailDeliveryStates.ON) return VerificationBases.LOCAL_CREDENTIAL;
	return params.basis === VerificationBases.AUTO ? VerificationBases.CONFIRMED_EMAIL : params.basis;
}

export interface AccountTierInput {
	isBot: boolean;
	isSystem: boolean;
	traits: UserTraitSet;
	identityMode: AccountIdentityMode;
	effectiveBasis: EffectiveVerificationBasis;
	hasEmail: boolean;
	emailVerified: boolean;
	localCredential: LocalCredentialKind;
}

export function resolveAccountTier(input: AccountTierInput): AccountTier {
	if (input.isBot || input.isSystem || hasUserTrait(input.traits, UserTraits.SSO)) return AccountTiers.VERIFIED;
	const hasCredential = input.localCredential !== LocalCredentialKinds.NONE;
	if (input.identityMode === AccountIdentityModes.USERNAME) {
		return hasCredential ? AccountTiers.VERIFIED : AccountTiers.UNCLAIMED;
	}
	if (!input.hasEmail) return AccountTiers.UNCLAIMED;
	if (input.effectiveBasis === VerificationBases.LOCAL_CREDENTIAL) {
		return hasCredential ? AccountTiers.VERIFIED : AccountTiers.UNCLAIMED;
	}
	return input.emailVerified ? AccountTiers.VERIFIED : AccountTiers.UNVERIFIED;
}

export function resolveVerificationNeed(params: {
	tier: AccountTier;
	identityMode: AccountIdentityMode;
	hasEmail: boolean;
}): AccountVerificationNeed | null {
	switch (params.tier) {
		case AccountTiers.VERIFIED:
			return null;
		case AccountTiers.UNVERIFIED:
			return AccountVerificationNeeds.CONFIRM_EMAIL;
		case AccountTiers.UNCLAIMED:
			return params.identityMode === AccountIdentityModes.USERNAME || params.hasEmail
				? AccountVerificationNeeds.SET_PASSWORD
				: AccountVerificationNeeds.ADD_EMAIL;
	}
}

interface TierRule {
	unclaimed: TierDenialError | null;
	unverified: TierDenialError | null;
	exemptions?: ReadonlyArray<TierExemption>;
	onlyWhenGuildJoinGateApplies?: true;
}

const ALLOWED: TierRule = {unclaimed: null, unverified: null};
const VERIFICATION_REQUIRED: TierRule = {
	unclaimed: TierDenialErrors.ACCOUNT_VERIFICATION_REQUIRED,
	unverified: TierDenialErrors.ACCOUNT_VERIFICATION_REQUIRED,
};

const SHARED_RULES = {
	[TierGatedActions.REACTION]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_ADD_REACTIONS,
		unverified: TierDenialErrors.REACTION_EMAIL_VERIFICATION_REQUIRED,
	},
	[TierGatedActions.FRIEND_REQUEST]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_SEND_FRIEND_REQUESTS,
		unverified: TierDenialErrors.FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED,
	},
	[TierGatedActions.GUILD_CREATE]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_CREATE_GUILDS,
		unverified: TierDenialErrors.GUILD_CREATION_EMAIL_VERIFICATION_REQUIRED,
	},
	[TierGatedActions.GIFT_REDEEM]: {unclaimed: TierDenialErrors.ACCOUNT_VERIFICATION_REQUIRED, unverified: null},
	[TierGatedActions.REPORT]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_SUBMIT_REPORTS,
		unverified: TierDenialErrors.REPORT_EMAIL_VERIFICATION_REQUIRED,
	},
	[TierGatedActions.MFA]: {
		unclaimed: TierDenialErrors.MFA_EMAIL_VERIFICATION_REQUIRED,
		unverified: TierDenialErrors.MFA_EMAIL_VERIFICATION_REQUIRED,
	},
	[TierGatedActions.GUILD_VERIFICATION]: {
		unclaimed: TierDenialErrors.GUILD_VERIFICATION_REQUIRED,
		unverified: TierDenialErrors.GUILD_EMAIL_VERIFICATION_REQUIRED,
	},
} as const satisfies Partial<Record<TierGatedAction, TierRule>>;

const LEGACY_RULES: Record<TierGatedAction, TierRule> = {
	...SHARED_RULES,
	[TierGatedActions.MESSAGE]: {unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES, unverified: null},
	[TierGatedActions.TYPING]: ALLOWED,
	[TierGatedActions.DIRECT_MESSAGE]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES,
		unverified: null,
	},
	[TierGatedActions.DIRECT_MESSAGE_OPEN]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES,
		unverified: TierDenialErrors.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED,
	},
	[TierGatedActions.GROUP_DM]: {unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_JOIN_GROUP_DMS, unverified: null},
	[TierGatedActions.FRIEND_ACCEPT]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_ACCEPT_FRIEND_REQUESTS,
		unverified: null,
	},
	[TierGatedActions.INVITE_CREATE]: ALLOWED,
	[TierGatedActions.GUILD_JOIN]: ALLOWED,
	[TierGatedActions.VOICE]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS,
		unverified: null,
		exemptions: [TierExemptions.GUILD_OWNER],
	},
	[TierGatedActions.CALL]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_JOIN_ONE_ON_ONE_VOICE_CALLS,
		unverified: null,
	},
	[TierGatedActions.GROUP_CALL]: ALLOWED,
	[TierGatedActions.THREAD_JOIN]: ALLOWED,
	[TierGatedActions.CHANNEL_FOLLOW]: ALLOWED,
	[TierGatedActions.OWNERSHIP_TRANSFER]: ALLOWED,
	[TierGatedActions.APPLICATION_CREATE]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_CREATE_APPLICATIONS,
		unverified: null,
	},
	[TierGatedActions.PROFILE_CUSTOMIZE]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNTS_CAN_ONLY_SET_EMAIL_VIA_TOKEN,
		unverified: TierDenialErrors.PROFILE_EMAIL_VERIFICATION_REQUIRED,
	},
};

const ENFORCED_CALL_RULE: TierRule = {
	unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_JOIN_ONE_ON_ONE_VOICE_CALLS,
	unverified: TierDenialErrors.ACCOUNT_VERIFICATION_REQUIRED,
};

const ENFORCED_MESSAGE_RULE: TierRule = {
	unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES,
	unverified: TierDenialErrors.EMAIL_VERIFICATION_REQUIRED,
};

const ENFORCED_DIRECT_MESSAGE_RULE: TierRule = {
	unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES,
	unverified: TierDenialErrors.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED,
};

const ENFORCED_RULES: Record<TierGatedAction, TierRule> = {
	...SHARED_RULES,
	[TierGatedActions.MESSAGE]: ENFORCED_MESSAGE_RULE,
	[TierGatedActions.TYPING]: ENFORCED_MESSAGE_RULE,
	[TierGatedActions.DIRECT_MESSAGE]: ENFORCED_DIRECT_MESSAGE_RULE,
	[TierGatedActions.DIRECT_MESSAGE_OPEN]: ENFORCED_DIRECT_MESSAGE_RULE,
	[TierGatedActions.GROUP_DM]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_JOIN_GROUP_DMS,
		unverified: TierDenialErrors.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED,
	},
	[TierGatedActions.FRIEND_ACCEPT]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_ACCEPT_FRIEND_REQUESTS,
		unverified: TierDenialErrors.FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED,
	},
	[TierGatedActions.INVITE_CREATE]: VERIFICATION_REQUIRED,
	[TierGatedActions.GUILD_JOIN]: {...VERIFICATION_REQUIRED, onlyWhenGuildJoinGateApplies: true},
	[TierGatedActions.VOICE]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS,
		unverified: TierDenialErrors.ACCOUNT_VERIFICATION_REQUIRED,
	},
	[TierGatedActions.CALL]: ENFORCED_CALL_RULE,
	[TierGatedActions.GROUP_CALL]: ENFORCED_CALL_RULE,
	[TierGatedActions.THREAD_JOIN]: VERIFICATION_REQUIRED,
	[TierGatedActions.CHANNEL_FOLLOW]: VERIFICATION_REQUIRED,
	[TierGatedActions.OWNERSHIP_TRANSFER]: VERIFICATION_REQUIRED,
	[TierGatedActions.APPLICATION_CREATE]: {
		unclaimed: TierDenialErrors.UNCLAIMED_ACCOUNT_CANNOT_CREATE_APPLICATIONS,
		unverified: TierDenialErrors.ACCOUNT_VERIFICATION_REQUIRED,
	},
	[TierGatedActions.PROFILE_CUSTOMIZE]: ALLOWED,
};

const TIER_RULES: Readonly<Record<InteractionTierMode, Readonly<Record<TierGatedAction, TierRule>>>> = {
	[InteractionTierModes.LEGACY]: LEGACY_RULES,
	[InteractionTierModes.ENFORCED]: ENFORCED_RULES,
};

export interface TierGateInput {
	tier: AccountTier;
	mode: InteractionTierMode;
	action: TierGatedAction;
	identityMode: AccountIdentityMode;
	hasEmail: boolean;
	guildJoinGateApplies: boolean;
	exemptions: ReadonlyArray<TierExemption>;
}

export interface TierGateDenial {
	allowed: false;
	action: TierGatedAction;
	tier: RestrictedAccountTier;
	need: AccountVerificationNeed;
	errorCode: TierDenialError;
}

export type TierGateResult = {allowed: true} | TierGateDenial;

const TIER_GATE_ALLOWED: TierGateResult = {allowed: true};

export function evaluateTierGate(input: TierGateInput): TierGateResult {
	const {tier} = input;
	if (tier === AccountTiers.VERIFIED) return TIER_GATE_ALLOWED;
	const rule = TIER_RULES[input.mode][input.action];
	if (rule.onlyWhenGuildJoinGateApplies && !input.guildJoinGateApplies) return TIER_GATE_ALLOWED;
	if (rule.exemptions?.some((exemption) => input.exemptions.includes(exemption))) return TIER_GATE_ALLOWED;
	const denial = tier === AccountTiers.UNCLAIMED ? rule.unclaimed : rule.unverified;
	if (denial === null) return TIER_GATE_ALLOWED;
	const need = resolveVerificationNeed({tier, identityMode: input.identityMode, hasEmail: input.hasEmail});
	if (need === null) return TIER_GATE_ALLOWED;
	return {
		allowed: false,
		action: input.action,
		tier,
		need,
		errorCode: need === AccountVerificationNeeds.SET_PASSWORD ? TierDenialErrors.ACCOUNT_VERIFICATION_REQUIRED : denial,
	};
}

export function resolveEmailDeliveryState(params: {
	identityMode: AccountIdentityMode;
	delivery: EmailDeliveryMode;
	smtpComplete: boolean;
	fromAddress: string | null;
}): EmailDeliveryState {
	if (params.identityMode === AccountIdentityModes.USERNAME) return EmailDeliveryStates.OFF_USERNAME_MODE;
	if (params.delivery === EmailDeliveryModes.OFF) return EmailDeliveryStates.OFF_DISABLED;
	if (!params.smtpComplete || !params.fromAddress?.trim()) return EmailDeliveryStates.OFF_INCOMPLETE;
	return EmailDeliveryStates.ON;
}

export function resolveEmailCodeAvailability(params: {
	identityMode: AccountIdentityMode;
	deliveryState: EmailDeliveryState;
	hasEmail: boolean;
	emailVerified: boolean;
}): EmailCodeAvailability {
	if (params.identityMode === AccountIdentityModes.USERNAME) return EmailCodeAvailability.IDENTITY_MODE_USERNAME;
	if (params.deliveryState !== EmailDeliveryStates.ON) return EmailCodeAvailability.DELIVERY_OFF;
	if (!params.hasEmail || !params.emailVerified) return EmailCodeAvailability.NO_OWNED_EMAIL;
	return EmailCodeAvailability.AVAILABLE;
}

export function resolveIdentifierFirst(params: {
	mode: IdentifierFirstMode;
	identityMode: AccountIdentityMode;
	deliveryState: EmailDeliveryState;
}): {identifierFirst: boolean; emailCodes: boolean} {
	const emailMode = params.identityMode === AccountIdentityModes.EMAIL;
	const deliveryOn = params.deliveryState === EmailDeliveryStates.ON;
	let identifierFirst: boolean;
	switch (params.mode) {
		case IdentifierFirstModes.OFF:
			identifierFirst = false;
			break;
		case IdentifierFirstModes.ON:
			identifierFirst = emailMode;
			break;
		case IdentifierFirstModes.AUTO:
			identifierFirst = emailMode && deliveryOn;
			break;
	}
	return {identifierFirst, emailCodes: identifierFirst && deliveryOn};
}

export function resolveInstantAccountAvailability(params: {
	enabled: boolean;
	captchaProvider: CaptchaProvider;
	registrationMode: RegistrationMode;
	registrationUrl: {approvalRequired: boolean} | null;
}): InstantAccountAvailability {
	if (!params.enabled) return InstantAccountAvailability.DISABLED;
	if (params.captchaProvider === CaptchaProviders.NONE) return InstantAccountAvailability.CAPTCHA_OFF;
	if (params.registrationUrl !== null) {
		return params.registrationUrl.approvalRequired
			? InstantAccountAvailability.APPROVAL_REQUIRED
			: InstantAccountAvailability.AVAILABLE;
	}
	switch (params.registrationMode) {
		case RegistrationModes.OPEN:
			return InstantAccountAvailability.AVAILABLE;
		case RegistrationModes.APPROVAL:
			return InstantAccountAvailability.APPROVAL_REQUIRED;
		case RegistrationModes.CLOSED:
			return InstantAccountAvailability.REGISTRATION_CLOSED;
	}
}

export function requiresLocalCredential(params: {
	identityMode: AccountIdentityMode;
	deliveryState: EmailDeliveryState;
	emailProven: boolean;
}): boolean {
	if (params.identityMode === AccountIdentityModes.USERNAME) return true;
	if (params.deliveryState === EmailDeliveryStates.ON) return false;
	return !params.emailProven;
}

export function resolvePurchaseBlockedReason(params: {tier: AccountTier}): PurchaseBlockedReason | null {
	switch (params.tier) {
		case AccountTiers.UNCLAIMED:
			return PurchaseBlockedReasons.UNCLAIMED;
		case AccountTiers.UNVERIFIED:
			return PurchaseBlockedReasons.EMAIL_UNVERIFIED;
		case AccountTiers.VERIFIED:
			return null;
	}
}
