// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import CallState from '@app/features/voice/state/CallState';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

export function isActiveCallParticipant(channel: Channel | null | undefined, userId: string): boolean {
	if (!channel?.isPrivate()) return false;
	const call = CallState.getCall(channel.id);
	if (!call) return false;
	const participants = new Set([...call.participants, ...CallState.getParticipants(channel.id)]);
	return participants.has(userId);
}

export function hasActiveDirectCallWithUser(userId: string): boolean {
	return Channels.dmChannels.some((channel) => {
		return (
			channel.type === ChannelTypes.DM && channel.recipientIds.includes(userId) && CallState.hasActiveCall(channel.id)
		);
	});
}
