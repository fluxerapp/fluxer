/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import React from 'react';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';

export interface ConnectedVoiceSession {
	guildId: string | null;
	channelId: string | null;
	channel: ChannelRecord | null;
	guild: GuildRecord | null;
	isConnected: boolean;
}

export const useConnectedVoiceSession = (): ConnectedVoiceSession => {
	const channelId = MediaEngineStore.channelId;
	const guildId = MediaEngineStore.guildId;

	const channel = React.useMemo(() => (channelId ? (ChannelStore.getChannel(channelId) ?? null) : null), [channelId]);
	const guild = React.useMemo(() => (guildId ? (GuildStore.getGuild(guildId) ?? null) : null), [guildId]);

	return {
		channel,
		channelId,
		guild,
		guildId,
		isConnected: Boolean(channel && guild),
	};
};
