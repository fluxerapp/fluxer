// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import VoiceRuntime, {useVoiceRuntimeVersion} from '@app/features/voice/engine/VoiceRuntime';

export interface ConnectedVoiceSession {
	guildId: string | null;
	channelId: string | null;
	channel: Channel | null;
	guild: Guild | null;
	isConnected: boolean;
}

export const useConnectedVoiceSession = (): ConnectedVoiceSession => {
	useVoiceRuntimeVersion();
	const channelId = VoiceRuntime.channelId;
	const guildId = VoiceRuntime.guildId;
	const channel = channelId ? (Channels.getChannel(channelId) ?? null) : null;
	const guild = guildId ? (Guilds.getGuild(guildId) ?? null) : null;
	return {
		channel,
		channelId,
		guild,
		guildId,
		isConnected: Boolean(channel),
	};
};
