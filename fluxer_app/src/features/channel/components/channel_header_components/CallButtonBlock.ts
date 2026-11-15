// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {isSystemDmChannel} from '@app/features/channel/utils/ChannelUtils';
import Users from '@app/features/user/state/Users';
import {type AccountGateUser, canPerform} from '@app/features/user/utils/AccountGates';
import {TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';
import type {TierGateDenial} from '@fluxer/constants/src/AccountTierRules';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

export const CallButtonBlocks = {
	NONE: 'none',
	SYSTEM_DM: 'system_dm',
	ACCOUNT: 'account',
} as const;

export type CallButtonBlockState =
	| {readonly block: typeof CallButtonBlocks.NONE}
	| {readonly block: typeof CallButtonBlocks.SYSTEM_DM}
	| {readonly block: typeof CallButtonBlocks.ACCOUNT; readonly gate: TierGateDenial};

export function resolveCallButtonBlock(
	channel: Channel,
	user: AccountGateUser | null | undefined,
): CallButtonBlockState {
	if (isSystemDmChannel(channel)) return {block: CallButtonBlocks.SYSTEM_DM};
	const action = channel.type === ChannelTypes.DM ? TierGatedActions.CALL : TierGatedActions.GROUP_CALL;
	const gate = canPerform(user, action);
	if (gate.allowed) return {block: CallButtonBlocks.NONE};
	return {block: CallButtonBlocks.ACCOUNT, gate};
}

export function useCallButtonBlock(channel: Channel): CallButtonBlockState {
	return resolveCallButtonBlock(channel, Users.getCurrentUser());
}
