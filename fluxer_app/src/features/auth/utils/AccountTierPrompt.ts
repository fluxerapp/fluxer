// SPDX-License-Identifier: AGPL-3.0-or-later

import {failureCode, failureContext} from '@app/features/platform/utils/ResponseInspection';
import Users from '@app/features/user/state/Users';
import {
	type AccountGate,
	type AccountGateAction,
	AccountGateActions,
	openAccountGateFlow,
} from '@app/features/user/utils/AccountGatePresentation';
import {
	type AccountVerificationNeed,
	AccountVerificationNeeds,
	type TierDenialError,
	type TierDenialErrors,
	type TierGatedAction,
	TierGatedActions,
} from '@fluxer/constants/src/AccountTierConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {i18n} from '@lingui/core';

type LegacyTierCode =
	| Exclude<
			TierDenialError,
			| typeof TierDenialErrors.ACCOUNT_VERIFICATION_REQUIRED
			| typeof TierDenialErrors.UNCLAIMED_ACCOUNTS_CAN_ONLY_SET_EMAIL_VIA_TOKEN
	  >
	| typeof APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_MAKE_PURCHASES
	| typeof APIErrorCodes.PURCHASE_EMAIL_VERIFICATION_REQUIRED;

const LEGACY_TIER_CODE_ACTIONS: Readonly<Record<LegacyTierCode, AccountGateAction>> = {
	[APIErrorCodes.EMAIL_VERIFICATION_REQUIRED]: AccountGateActions.MESSAGE,
	[APIErrorCodes.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED]: AccountGateActions.DIRECT_MESSAGE_OPEN,
	[APIErrorCodes.FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED]: AccountGateActions.FRIEND_REQUEST,
	[APIErrorCodes.GUILD_CREATION_EMAIL_VERIFICATION_REQUIRED]: AccountGateActions.GUILD_CREATE,
	[APIErrorCodes.GUILD_EMAIL_VERIFICATION_REQUIRED]: AccountGateActions.GUILD_VERIFICATION,
	[APIErrorCodes.GUILD_VERIFICATION_REQUIRED]: AccountGateActions.GUILD_VERIFICATION,
	[APIErrorCodes.MFA_EMAIL_VERIFICATION_REQUIRED]: AccountGateActions.MFA,
	[APIErrorCodes.PROFILE_EMAIL_VERIFICATION_REQUIRED]: AccountGateActions.PROFILE_CUSTOMIZE,
	[APIErrorCodes.REACTION_EMAIL_VERIFICATION_REQUIRED]: AccountGateActions.REACTION,
	[APIErrorCodes.REPORT_EMAIL_VERIFICATION_REQUIRED]: AccountGateActions.REPORT,
	[APIErrorCodes.PURCHASE_EMAIL_VERIFICATION_REQUIRED]: AccountGateActions.PURCHASE,
	[APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_ACCEPT_FRIEND_REQUESTS]: AccountGateActions.FRIEND_ACCEPT,
	[APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_ADD_REACTIONS]: AccountGateActions.REACTION,
	[APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_CREATE_APPLICATIONS]: AccountGateActions.APPLICATION_CREATE,
	[APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_CREATE_GUILDS]: AccountGateActions.GUILD_CREATE,
	[APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_JOIN_GROUP_DMS]: AccountGateActions.GROUP_DM,
	[APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_JOIN_ONE_ON_ONE_VOICE_CALLS]: AccountGateActions.CALL,
	[APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS]: AccountGateActions.VOICE,
	[APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_MAKE_PURCHASES]: AccountGateActions.PURCHASE,
	[APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES]: AccountGateActions.DIRECT_MESSAGE,
	[APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SEND_FRIEND_REQUESTS]: AccountGateActions.FRIEND_REQUEST,
	[APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES]: AccountGateActions.MESSAGE,
	[APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_SUBMIT_REPORTS]: AccountGateActions.REPORT,
};

const TIER_GATED_ACTION_VALUES: ReadonlySet<string> = new Set(Object.values(TierGatedActions));
const NEED_VALUES: ReadonlySet<string> = new Set(Object.values(AccountVerificationNeeds));

function isLegacyTierCode(code: string): code is LegacyTierCode {
	return Object.hasOwn(LEGACY_TIER_CODE_ACTIONS, code);
}

function isTierGatedAction(value: unknown): value is TierGatedAction {
	return typeof value === 'string' && TIER_GATED_ACTION_VALUES.has(value);
}

function isVerificationNeed(value: unknown): value is AccountVerificationNeed {
	return typeof value === 'string' && NEED_VALUES.has(value);
}

function describeVerificationRequired(error: unknown): AccountGate | null {
	const data = failureContext(error);
	const rawAction = data?.['action'];
	const rawNeed = data?.['need'];
	const action = isTierGatedAction(rawAction) ? rawAction : null;
	const need = isVerificationNeed(rawNeed) ? rawNeed : (Users.currentUser?.verificationNeed ?? null);
	if (action === null || need === null) return null;
	return {action, need};
}

export function describeAccountTierBlock(error: unknown): AccountGate | null {
	const code = failureCode(error);
	if (code === undefined) return null;
	if (code === APIErrorCodes.ACCOUNT_VERIFICATION_REQUIRED) return describeVerificationRequired(error);
	if (!isLegacyTierCode(code)) return null;
	const need = Users.currentUser?.verificationNeed ?? null;
	if (need === null) return null;
	return {action: LEGACY_TIER_CODE_ACTIONS[code], need};
}

export function promptForAccountGate(gate: AccountGate, retry?: () => void): boolean {
	if (Users.currentUser?.reportsAccountTier !== true) return false;
	openAccountGateFlow(i18n, gate, retry);
	return true;
}

export function promptForAccountTier(error: unknown, retry?: () => void): boolean {
	const gate = describeAccountTierBlock(error);
	return gate !== null && promptForAccountGate(gate, retry);
}
