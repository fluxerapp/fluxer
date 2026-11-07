// SPDX-License-Identifier: AGPL-3.0-or-later

import {GroupedVoiceParticipant} from '@app/features/app/components/layout/GroupedVoiceParticipant';
import {VoiceParticipantItem} from '@app/features/app/components/layout/VoiceParticipantItem';
import styles from '@app/features/app/components/layout/VoiceParticipantsList.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import type {Guild} from '@app/features/guild/models/Guild';
import Users from '@app/features/user/state/Users';
import {resolveVoiceParticipantDisplayState} from '@app/features/voice/components/VoiceParticipantDisplayState';
import {
	createVoiceParticipantSortSnapshot,
	sortVoiceParticipantItemsWithSnapshot,
} from '@app/features/voice/components/VoiceParticipantSortUtils';
import VoiceRuntime, {useVoiceRuntimeVersion} from '@app/features/voice/engine/VoiceRuntime';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import {isParticipantVoicePermissionMuted} from '@app/features/voice/utils/VoicePermissionUtils';
import {observer} from 'mobx-react-lite';
import {useMemo, useRef} from 'react';

export const VoiceParticipantsList = observer(({guild, channel}: {guild: Guild; channel: Channel}) => {
	const mediaEngineVersion = useVoiceRuntimeVersion();
	const voiceStates = VoiceRuntime.getAllVoiceStatesInChannel(guild.id, channel.id);
	const currentUser = Users.currentUser;
	const localSelfMute = LocalVoiceState.getSelfMute();
	const localSelfStream = LocalVoiceState.getSelfStream();
	const localConnectionId = VoiceRuntime.connectionId;
	const groupedSortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
	const grouped = useMemo(() => {
		const byUser = new Map<
			string,
			{
				userId: string;
				states: Array<VoiceState>;
				isCurrentUser: boolean;
				anySpeaking: boolean;
				anyLive: boolean;
			}
		>();
		for (const connectionKey in voiceStates) {
			const vs = voiceStates[connectionKey];
			if (!vs) continue;
			const userId = vs.user_id;
			let entry = byUser.get(userId);
			if (!entry) {
				entry = {userId, states: [], isCurrentUser: currentUser?.id === userId, anySpeaking: false, anyLive: false};
				byUser.set(userId, entry);
			}
			entry.states.push(vs);
			const connectionId = vs.connection_id ?? '';
			const participant = VoiceRuntime.getParticipantByUserIdAndConnectionId(userId, connectionId);
			const isLocalConnection = entry.isCurrentUser && connectionId === localConnectionId;
			const isPermissionMuted = isParticipantVoicePermissionMuted({
				voiceState: vs,
				guildId: guild.id,
				channelId: channel.id,
				isCurrentUser: entry.isCurrentUser,
			});
			const displayState = resolveVoiceParticipantDisplayState({
				participant,
				voiceState: vs,
				isLocalConnection,
				localSelfMute,
				localSelfStream,
				permissionMuted: isPermissionMuted,
			});
			entry.anySpeaking = entry.anySpeaking || displayState.speaking;
			entry.anyLive = entry.anyLive || displayState.streaming;
			if (entry.isCurrentUser) {
				entry.anyLive = entry.anyLive || localSelfStream;
			}
		}
		return sortVoiceParticipantItemsWithSnapshot(Array.from(byUser.values()), {
			snapshot: groupedSortSnapshotRef.current,
			getParticipantKey: (entry) => entry.userId,
			getUserId: (entry) => entry.userId,
			guildId: guild.id,
			channelId: channel.id,
		});
	}, [
		channel.id,
		currentUser,
		guild.id,
		localConnectionId,
		localSelfMute,
		localSelfStream,
		mediaEngineVersion,
		voiceStates,
	]);
	if (grouped.length === 0) return null;
	return (
		<div className={styles.container} data-flx="app.voice-participants-list.container">
			{grouped.map(({userId, states, isCurrentUser, anySpeaking}) => {
				const user = Users.getUser(userId);
				if (!user) return null;
				if (states.length === 1) {
					return (
						<VoiceParticipantItem
							key={userId}
							user={user}
							voiceState={states[0]}
							guildId={guild.id}
							isCurrentUser={isCurrentUser}
							isCurrentUserConnection={isCurrentUser && states[0].connection_id === localConnectionId}
							data-flx="app.voice-participants-list.voice-participant-item"
						/>
					);
				}
				return (
					<GroupedVoiceParticipant
						key={userId}
						user={user}
						voiceStates={states}
						guildId={guild.id}
						anySpeaking={anySpeaking}
						data-flx="app.voice-participants-list.grouped-voice-participant"
					/>
				);
			})}
		</div>
	);
});
