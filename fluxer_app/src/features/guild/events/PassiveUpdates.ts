// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import Messages from '@app/features/messaging/state/MessagingMessages';
import ReadStates from '@app/features/read_state/state/ReadStates';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import VoiceRuntime from '@app/features/voice/engine/VoiceRuntime';

export interface PassiveUpdatesPayload {
	guild_id: string;
	channels: Record<string, string>;
	voice_states?: Array<VoiceState>;
}

export function handlePassiveUpdates(data: PassiveUpdatesPayload, _context: GatewayHandlerContext): void {
	const {channels, voice_states: voiceStates} = data;
	const changedChannels = Channels.handlePassiveLastMessageUpdates({
		guildId: data.guild_id,
		channels,
	});
	ReadStates.handlePassiveLastMessageUpdates(channels, data.guild_id);
	Messages.handlePassiveLastMessageUpdates(channels);
	if (changedChannels) {
		QuickSwitcher.recomputeIfOpen();
	}
	if (voiceStates?.length) {
		VoiceRuntime.handlePassiveVoiceStates(data.guild_id, voiceStates);
	}
}
