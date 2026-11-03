// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import Relationships from '@app/features/relationship/state/Relationships';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import type {NormalizedVoiceState} from '@app/features/voice/engine/VoiceGatewayStateMachine';
import type {UserVoiceActivity} from '@app/features/voice/hooks/useUserVoiceActivities';
import {ME} from '@fluxer/constants/src/AppConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {useMemo} from 'react';

function collectParticipantUsers(channelStates: Readonly<Record<string, NormalizedVoiceState>>): {
	participantUserIds: Array<string>;
	participantUsers: Array<User>;
} {
	const participantUserIds: Array<string> = [];
	const participantUsers: Array<User> = [];
	const seenUserIds = new Set<string>();
	for (const connectionId in channelStates) {
		const voiceState = channelStates[connectionId];
		if (!voiceState?.user_id || seenUserIds.has(voiceState.user_id)) continue;
		seenUserIds.add(voiceState.user_id);
		participantUserIds.push(voiceState.user_id);
		const user = Users.getUser(voiceState.user_id);
		if (user) {
			participantUsers.push(user);
		}
	}
	return {participantUserIds, participantUsers};
}

export function useActiveFriendVoiceStates(): ReadonlyArray<UserVoiceActivity> {
	const relationships = Relationships.getRelationships();
	const allVoiceStates = MediaEngine.getAllVoiceStates();
	const currentUser = Users.getCurrentUser();
	const currentUserId = currentUser?.id ?? null;
	return useMemo(() => {
		const friendIds = new Set<string>();
		for (const rel of relationships) {
			if (rel.type === RelationshipTypes.FRIEND && rel.friendSharesVoiceActivity) {
				friendIds.add(rel.userId);
			}
		}
		const seenChannels = new Set<string>();
		const activities: Array<UserVoiceActivity> = [];
		for (const guildKey in allVoiceStates) {
			const guildStates = allVoiceStates[guildKey];
			if (!guildStates) continue;
			for (const channelId in guildStates) {
				const channelStates = guildStates[channelId];
				if (!channelStates) continue;
				if (seenChannels.has(channelId)) {
					continue;
				}
				let connectionId: string | null = null;
				let voiceState: NormalizedVoiceState | null = null;
				for (const stateConnectionId in channelStates) {
					const candidate = channelStates[stateConnectionId];
					if (!candidate) continue;
					if (currentUserId && candidate.user_id === currentUserId) {
						connectionId = stateConnectionId;
						voiceState = candidate;
						break;
					}
					if (friendIds.has(candidate.user_id) && candidate.channel_id) {
						connectionId = stateConnectionId;
						voiceState = candidate;
						break;
					}
				}
				if (connectionId == null || !voiceState) {
					continue;
				}
				seenChannels.add(channelId);
				const effectiveGuildId = guildKey === ME ? null : guildKey;
				const channel = Channels.getChannel(channelId);
				const guild = effectiveGuildId ? Guilds.getGuild(effectiveGuildId) : undefined;
				const isStreaming = voiceState.self_stream === true;
				let streamKey: string | null = null;
				if (isStreaming && effectiveGuildId) {
					streamKey = `${effectiveGuildId}:${channelId}:${connectionId}`;
				} else if (isStreaming) {
					streamKey = `dm:${channelId}:${connectionId}`;
				}
				const {participantUserIds, participantUsers} = collectParticipantUsers(channelStates);
				activities.push({
					voiceState,
					connectionId,
					guildId: effectiveGuildId,
					channelId,
					channel,
					guild,
					isStreaming,
					streamKey,
					participantUserIds,
					participantUsers,
				});
			}
		}
		activities.sort((a, b) => b.participantUsers.length - a.participantUsers.length);
		return activities;
	}, [relationships, allVoiceStates, currentUserId]);
}
