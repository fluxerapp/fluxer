// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import Permission from '@app/features/permissions/state/Permission';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import type {Channel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';

export type ChannelUpdatePayload = Partial<Channel> & {
	id: string;
	type: number;
};

export function handleChannelUpdate(data: ChannelUpdatePayload, _context: GatewayHandlerContext): void {
	const existing = Channels.getChannel(data.id);
	const channel = existing != null ? existing.withUpdates(data) : (data as Channel);
	Channels.handleChannelCreate({channel});
	Permission.handleChannelUpdate(data.id);
	GuildReadState.handleGenericUpdate(data.id);
	QuickSwitcher.recomputeIfOpen();
	if (data.bitrate !== undefined) {
		void MediaEngine.refreshMicrophonePublishSettingsForChannel(data.id);
	}
}
