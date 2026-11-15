// SPDX-License-Identifier: AGPL-3.0-or-later

import {channelIdToUserId} from '@app/api/BrandedTypes';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {GatewayDispatchEvent} from '@app/api/constants/Gateway';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {Channel} from '@app/api/models/Channel';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {THREAD_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';

interface DispatchChannelEventParams {
	gatewayService: IGatewayService;
	channel: Channel;
	event: GatewayDispatchEvent;
	data: unknown;
}

export function withThreadContext(channel: Channel, data: unknown): unknown {
	if (!THREAD_CHANNEL_TYPES.has(channel.type) || typeof data !== 'object' || data === null) return data;
	return {
		...data,
		_fluxer_thread: {id: channel.id.toString(), parent_id: channel.parentId?.toString() ?? null, type: channel.type},
	};
}

export async function dispatchChannelEvent({
	gatewayService,
	channel,
	event,
	data,
}: DispatchChannelEventParams): Promise<void> {
	if (channel.type === ChannelTypes.DM_PERSONAL_NOTES) {
		const userId = channelIdToUserId(channel.id);
		if (userId === SYSTEM_USER_ID) {
			return;
		}
		return gatewayService.dispatchPresence({
			userId,
			event,
			data,
		});
	}
	if (channel.guildId) {
		return gatewayService.dispatchGuild({guildId: channel.guildId, event, data: withThreadContext(channel, data)});
	}
	await Promise.all(
		Array.from(channel.recipientIds)
			.filter((recipientId) => recipientId !== SYSTEM_USER_ID)
			.map((recipientId) => gatewayService.dispatchPresence({userId: recipientId, event, data})),
	);
}
