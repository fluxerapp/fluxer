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

import {observer} from 'mobx-react-lite';
import React from 'react';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import LocalVoiceStateStore from '~/stores/LocalVoiceStateStore';
import UserStore from '~/stores/UserStore';
import type {VoiceState} from '~/stores/voice/MediaEngineFacade';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import {GroupedVoiceParticipant} from './GroupedVoiceParticipant';
import {VoiceParticipantItem} from './VoiceParticipantItem';
import styles from './VoiceParticipantsList.module.css';

export const VoiceParticipantsList = observer(({guild, channel}: {guild: GuildRecord; channel: ChannelRecord}) => {
	const voiceStates = MediaEngineStore.getAllVoiceStatesInChannel(guild.id, channel.id);
	const currentUser = UserStore.currentUser;
	const localSelfStream = LocalVoiceStateStore.selfStream;

	const grouped = React.useMemo(() => {
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

		for (const vs of Object.values(voiceStates)) {
			const userId = vs.user_id;
			let entry = byUser.get(userId);
			if (!entry) {
				entry = {userId, states: [], isCurrentUser: currentUser?.id === userId, anySpeaking: false, anyLive: false};
				byUser.set(userId, entry);
			}
			entry.states.push(vs);

			const connectionId = vs.connection_id ?? '';
			const participant = MediaEngineStore.getParticipantByUserIdAndConnectionId(userId, connectionId);
			const isSelfMuted = vs.self_mute ?? (participant ? !participant.isMicrophoneEnabled : false);
			const isGuildMuted = vs.mute ?? false;
			const speaking = !!(participant?.isSpeaking && !isSelfMuted && !isGuildMuted);
			const live = vs.self_stream === true || (participant ? participant.isScreenShareEnabled : false);

			entry.anySpeaking = entry.anySpeaking || speaking;
			entry.anyLive = entry.anyLive || live;

			if (entry.isCurrentUser) {
				entry.anyLive = entry.anyLive || localSelfStream;
			}
		}

		return Array.from(byUser.values()).sort((a, b) => {
			if (a.isCurrentUser !== b.isCurrentUser) return a.isCurrentUser ? -1 : 1;
			if (a.anyLive !== b.anyLive) return a.anyLive ? -1 : 1;
			if (a.anySpeaking !== b.anySpeaking) return a.anySpeaking ? -1 : 1;
			return a.userId.localeCompare(b.userId);
		});
	}, [voiceStates, currentUser, localSelfStream]);

	if (grouped.length === 0) return null;

	return (
		<div className={styles.container}>
			{grouped.map(({userId, states, isCurrentUser, anySpeaking}) => {
				const user = UserStore.getUser(userId);
				if (!user) return null;

				if (states.length === 1) {
					return (
						<VoiceParticipantItem
							key={userId}
							user={user}
							voiceState={states[0]}
							guildId={guild.id}
							isCurrentUser={isCurrentUser}
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
					/>
				);
			})}
		</div>
	);
});
