// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const AccountTiers = {
	UNCLAIMED: 'unclaimed',
	UNVERIFIED: 'unverified',
	VERIFIED: 'verified',
} as const;

export type AccountTier = ValueOf<typeof AccountTiers>;

export type RestrictedAccountTier = Exclude<AccountTier, typeof AccountTiers.VERIFIED>;

export const AccountVerificationNeeds = {
	ADD_EMAIL: 'add_email',
	CONFIRM_EMAIL: 'confirm_email',
	SET_PASSWORD: 'password',
} as const;

export type AccountVerificationNeed = ValueOf<typeof AccountVerificationNeeds>;

export const TierGatedActions = {
	MESSAGE: 'message',
	TYPING: 'typing',
	REACTION: 'reaction',
	DIRECT_MESSAGE: 'direct_message',
	DIRECT_MESSAGE_OPEN: 'direct_message_open',
	GROUP_DM: 'group_dm',
	FRIEND_REQUEST: 'friend_request',
	FRIEND_ACCEPT: 'friend_accept',
	INVITE_CREATE: 'invite_create',
	GUILD_JOIN: 'guild_join',
	GUILD_CREATE: 'guild_create',
	VOICE: 'voice',
	CALL: 'call',
	GROUP_CALL: 'group_call',
	THREAD_JOIN: 'thread_join',
	CHANNEL_FOLLOW: 'channel_follow',
	OWNERSHIP_TRANSFER: 'ownership_transfer',
	APPLICATION_CREATE: 'application_create',
	GIFT_REDEEM: 'gift_redeem',
	PROFILE_CUSTOMIZE: 'profile_customize',
	REPORT: 'report',
	MFA: 'mfa',
	GUILD_VERIFICATION: 'guild_verification',
} as const;

export type TierGatedAction = ValueOf<typeof TierGatedActions>;

export const TierExemptions = {
	GUILD_OWNER: 'guild_owner',
} as const;

export type TierExemption = ValueOf<typeof TierExemptions>;

export const LocalCredentialKinds = {
	NONE: 'none',
	PASSWORD: 'password',
	PASSKEY: 'passkey',
	PASSWORD_AND_PASSKEY: 'password_and_passkey',
} as const;

export type LocalCredentialKind = ValueOf<typeof LocalCredentialKinds>;

export function localCredentialKindOf(params: {hasPassword: boolean; hasPasskey: boolean}): LocalCredentialKind {
	if (params.hasPassword && params.hasPasskey) return LocalCredentialKinds.PASSWORD_AND_PASSKEY;
	if (params.hasPassword) return LocalCredentialKinds.PASSWORD;
	if (params.hasPasskey) return LocalCredentialKinds.PASSKEY;
	return LocalCredentialKinds.NONE;
}

export const TierDenialErrors = {
	ACCOUNT_VERIFICATION_REQUIRED: APIErrorCodes.ACCOUNT_VERIFICATION_REQUIRED,
	EMAIL_VERIFICATION_REQUIRED: APIErrorCodes.EMAIL_VERIFICATION_REQUIRED,
	DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED: APIErrorCodes.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED,
	FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED: APIErrorCodes.FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED,
	GUILD_CREATION_EMAIL_VERIFICATION_REQUIRED: APIErrorCodes.GUILD_CREATION_EMAIL_VERIFICATION_REQUIRED,
	GUILD_EMAIL_VERIFICATION_REQUIRED: APIErrorCodes.GUILD_EMAIL_VERIFICATION_REQUIRED,
	GUILD_VERIFICATION_REQUIRED: APIErrorCodes.GUILD_VERIFICATION_REQUIRED,
	MFA_EMAIL_VERIFICATION_REQUIRED: APIErrorCodes.MFA_EMAIL_VERIFICATION_REQUIRED,
	PROFILE_EMAIL_VERIFICATION_REQUIRED: APIErrorCodes.PROFILE_EMAIL_VERIFICATION_REQUIRED,
	REACTION_EMAIL_VERIFICATION_REQUIRED: APIErrorCodes.REACTION_EMAIL_VERIFICATION_REQUIRED,
	REPORT_EMAIL_VERIFICATION_REQUIRED: APIErrorCodes.REPORT_EMAIL_VERIFICATION_REQUIRED,
	UNCLAIMED_ACCOUNT_CANNOT_ACCEPT_FRIEND_REQUESTS: APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_ACCEPT_FRIEND_REQUESTS,
	UNCLAIMED_ACCOUNT_CANNOT_ADD_REACTIONS: APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_ADD_REACTIONS,
	UNCLAIMED_ACCOUNT_CANNOT_CREATE_APPLICATIONS: APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_CREATE_APPLICATIONS,
	UNCLAIMED_ACCOUNT_CANNOT_CREATE_GUILDS: APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_CREATE_GUILDS,
	UNCLAIMED_ACCOUNT_CANNOT_JOIN_GROUP_DMS: APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_JOIN_GROUP_DMS,
	UNCLAIMED_ACCOUNT_CANNOT_JOIN_ONE_ON_ONE_VOICE_CALLS:
		APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_JOIN_ONE_ON_ONE_VOICE_CALLS,
	UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS: APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS,
	UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES: APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES,
	UNCLAIMED_ACCOUNT_CANNOT_SEND_FRIEND_REQUESTS: APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SEND_FRIEND_REQUESTS,
	UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES: APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES,
	UNCLAIMED_ACCOUNT_CANNOT_SUBMIT_REPORTS: APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SUBMIT_REPORTS,
	UNCLAIMED_ACCOUNTS_CAN_ONLY_SET_EMAIL_VIA_TOKEN: ValidationErrorCodes.UNCLAIMED_ACCOUNTS_CAN_ONLY_SET_EMAIL_VIA_TOKEN,
} as const;

export type TierDenialError = ValueOf<typeof TierDenialErrors>;

export const PurchaseBlockedReasons = {
	UNCLAIMED: 'unclaimed',
	EMAIL_UNVERIFIED: 'email_unverified',
} as const;

export type PurchaseBlockedReason = ValueOf<typeof PurchaseBlockedReasons>;
