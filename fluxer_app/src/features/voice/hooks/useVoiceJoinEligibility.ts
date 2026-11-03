// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import Channels from '@app/features/channel/state/Channels';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Permission from '@app/features/permissions/state/Permission';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';

interface VoiceJoinEligibility {
	canJoin: boolean;
}

export function useVoiceJoinEligibility({
	guildId,
	channelId,
}: {
	guildId: string | null;
	channelId: string | null;
}): VoiceJoinEligibility {
	useMediaEngineVersion();
	if (!channelId) return {canJoin: false};
	const channel = Channels.getChannel(channelId);
	if (!channel) return {canJoin: false};
	const effectiveGuildId = guildId ?? channel.guildId ?? null;
	const currentUserId = Authentication.currentUserId;
	if (effectiveGuildId && currentUserId) {
		const member = GuildMembers.getMember(effectiveGuildId, currentUserId);
		if (member?.isTimedOut()) return {canJoin: false};
	}
	if (effectiveGuildId && !Permission.can(Permissions.CONNECT, channel)) return {canJoin: false};
	if (effectiveGuildId && channel.userLimit && channel.userLimit > 0 && currentUserId) {
		const voiceStates = MediaEngine.getAllVoiceStatesInChannel(effectiveGuildId, channelId);
		const currentConnectionId = MediaEngine.connectionId;
		let adjustedCount = 0;
		for (const connectionId in voiceStates) {
			const voiceState = voiceStates[connectionId];
			if (!voiceState) continue;
			if (voiceState.connection_id === currentConnectionId) continue;
			adjustedCount += 1;
			if (adjustedCount >= channel.userLimit) return {canJoin: false};
		}
		if (adjustedCount >= channel.userLimit) return {canJoin: false};
	}
	return {canJoin: true};
}
