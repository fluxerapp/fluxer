// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import type {NormalizedVoiceState} from '@app/features/voice/engine/VoiceGatewayStateMachine';
import {ME} from '@fluxer/constants/src/AppConstants';
import {useMemo} from 'react';

export interface UserVoiceActivity {
	voiceState: VoiceState;
	connectionId: string;
	guildId: string | null;
	channelId: string;
	channel: Channel | undefined;
	guild: Guild | undefined;
	isStreaming: boolean;
	streamKey: string | null;
	participantUserIds: ReadonlyArray<string>;
	participantUsers: ReadonlyArray<User>;
}

export interface UserVoiceActivityAggregate {
	aggregateKey: string;
	primaryActivity: UserVoiceActivity;
	activities: ReadonlyArray<UserVoiceActivity>;
}

function compareVoiceActivitiesForPrimary(a: UserVoiceActivity, b: UserVoiceActivity): number {
	if (a.isStreaming !== b.isStreaming) {
		return a.isStreaming ? -1 : 1;
	}
	return a.connectionId.localeCompare(b.connectionId);
}

function buildAggregateKey(guildId: string | null, channelId: string): string {
	return `${guildId ?? 'dm'}:${channelId}`;
}

function collectParticipantUsers(channelStates: Readonly<Record<string, NormalizedVoiceState>>): {
	participantUserIds: Array<string>;
	participantUsers: Array<User>;
} {
	const participantUserIds: Array<string> = [];
	const participantUsers: Array<User> = [];
	const seenUserIds = new Set<string>();
	for (const connectionId in channelStates) {
		const participantState = channelStates[connectionId];
		if (!participantState?.user_id || seenUserIds.has(participantState.user_id)) continue;
		seenUserIds.add(participantState.user_id);
		participantUserIds.push(participantState.user_id);
		const participantUser = Users.getUser(participantState.user_id);
		if (participantUser) {
			participantUsers.push(participantUser);
		}
	}
	return {participantUserIds, participantUsers};
}

function useUserVoiceActivities(userId: string): ReadonlyArray<UserVoiceActivity> {
	useMediaEngineVersion();
	const allVoiceStates = MediaEngine.getAllVoiceStates();
	return useMemo(() => {
		const activities: Array<UserVoiceActivity> = [];
		for (const guildKey in allVoiceStates) {
			const guildStates = allVoiceStates[guildKey];
			if (!guildStates) continue;
			for (const channelId in guildStates) {
				const channelStates = guildStates[channelId];
				if (!channelStates) continue;
				for (const connectionId in channelStates) {
					const vs = channelStates[connectionId];
					if (!vs) continue;
					if (vs.user_id !== userId || !vs.channel_id) continue;
					const effectiveGuildId = guildKey === ME ? null : guildKey;
					const channel = Channels.getChannel(channelId);
					const guild = effectiveGuildId ? Guilds.getGuild(effectiveGuildId) : undefined;
					const isStreaming = vs.self_stream === true;
					const streamKey = isStreaming
						? effectiveGuildId
							? `${effectiveGuildId}:${channelId}:${connectionId}`
							: `dm:${channelId}:${connectionId}`
						: null;
					const {participantUserIds, participantUsers} = collectParticipantUsers(channelStates);
					activities.push({
						voiceState: vs,
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
		}
		return activities;
	}, [allVoiceStates, userId]);
}

export function useUserVoiceActivityAggregates(userId: string): ReadonlyArray<UserVoiceActivityAggregate> {
	const activities = useUserVoiceActivities(userId);
	const selectedGuildId = SelectedGuild.selectedGuildId;
	return useMemo(() => {
		const groupedActivities = new Map<string, Array<UserVoiceActivity>>();
		for (const activity of activities) {
			const aggregateKey = buildAggregateKey(activity.guildId, activity.channelId);
			const existing = groupedActivities.get(aggregateKey);
			if (existing) {
				existing.push(activity);
				continue;
			}
			groupedActivities.set(aggregateKey, [activity]);
		}
		const aggregates = Array.from(groupedActivities.entries()).map(([aggregateKey, grouped]) => {
			const orderedActivities = grouped.length > 1 ? [...grouped].sort(compareVoiceActivitiesForPrimary) : grouped;
			const primaryActivity = orderedActivities[0];
			return {
				aggregateKey,
				primaryActivity,
				activities: orderedActivities,
			};
		});
		aggregates.sort((a, b) => {
			const aGuildId = a.primaryActivity.guildId;
			const bGuildId = b.primaryActivity.guildId;
			const aInSelectedGuild = selectedGuildId != null && aGuildId === selectedGuildId;
			const bInSelectedGuild = selectedGuildId != null && bGuildId === selectedGuildId;
			if (aInSelectedGuild !== bInSelectedGuild) {
				return aInSelectedGuild ? -1 : 1;
			}
			if (a.primaryActivity.isStreaming !== b.primaryActivity.isStreaming) {
				return a.primaryActivity.isStreaming ? -1 : 1;
			}
			if ((aGuildId == null) !== (bGuildId == null)) {
				return aGuildId == null ? 1 : -1;
			}
			if (aGuildId != null && bGuildId != null) {
				const guildSort = aGuildId.localeCompare(bGuildId);
				if (guildSort !== 0) {
					return guildSort;
				}
			}
			return a.primaryActivity.channelId.localeCompare(b.primaryActivity.channelId);
		});
		return aggregates;
	}, [activities, selectedGuildId]);
}
