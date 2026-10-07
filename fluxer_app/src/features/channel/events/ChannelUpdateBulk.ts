// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import Permission from '@app/features/permissions/state/Permission';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import type {Channel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';

export type ChannelBulkUpdatePayload = Partial<Channel> & {
	id: string;
	type: number;
};

export interface ChannelUpdateBulkPayload {
	channels: Array<ChannelBulkUpdatePayload>;
}

export function handleChannelUpdateBulk(data: ChannelUpdateBulkPayload, _context: GatewayHandlerContext): void {
	for (const payload of data.channels) {
		const existing = Channels.getChannel(payload.id);
		const channel = existing != null ? existing.withUpdates(payload) : (payload as Channel);
		Channels.handleChannelCreate({channel});
		Permission.handleChannelUpdate(payload.id);
		GuildReadState.handleGenericUpdate(payload.id);
		if (payload.bitrate !== undefined) {
			void MediaEngine.refreshMicrophonePublishSettingsForChannel(payload.id);
		}
	}
	QuickSwitcher.recomputeIfOpen();
}
