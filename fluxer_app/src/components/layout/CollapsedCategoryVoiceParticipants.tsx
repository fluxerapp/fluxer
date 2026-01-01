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

import {SpeakerHighIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {AvatarStack} from '~/components/uikit/avatars/AvatarStack';
import {StackUserAvatar} from '~/components/uikit/avatars/StackUserAvatar';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import styles from './CollapsedCategoryVoiceParticipants.module.css';

export const CollapsedCategoryVoiceParticipants = observer(
	({guild, voiceChannels}: {guild: GuildRecord; voiceChannels: Array<ChannelRecord>}) => {
		const allVoiceStates = MediaEngineStore.getAllVoiceStates();

		const userIds = React.useMemo(() => {
			const ids = new Set<string>();
			for (const channel of voiceChannels) {
				const states = allVoiceStates[guild.id]?.[channel.id];
				if (!states) continue;
				for (const s of Object.values(states)) ids.add(s.user_id);
			}
			return Array.from(ids).sort((a, b) => a.localeCompare(b));
		}, [voiceChannels, allVoiceStates, guild.id]);

		if (userIds.length === 0) return null;

		const firstChannelForUser = (uid: string): ChannelRecord | undefined =>
			voiceChannels.find((ch) => {
				const states = allVoiceStates[guild.id]?.[ch.id];
				return !!states && Object.values(states).some((s) => s.user_id === uid);
			});

		return (
			<div className={styles.container}>
				<SpeakerHighIcon className={styles.icon} />
				<AvatarStack size={28} maxVisible={7}>
					{userIds.map((uid) => {
						const ch = firstChannelForUser(uid);
						if (!ch) return null;
						return <StackUserAvatar key={uid} guild={guild} channel={ch} userId={uid} />;
					})}
				</AvatarStack>
			</div>
		);
	},
);

export const CollapsedChannelAvatarStack = observer(
	({guild, channel}: {guild: GuildRecord; channel: ChannelRecord}) => {
		const channelStates = MediaEngineStore.getAllVoiceStatesInChannel(guild.id, channel.id);

		const uniqueUserIds = React.useMemo(() => {
			const set = new Set<string>();
			for (const s of Object.values(channelStates)) set.add(s.user_id);
			return Array.from(set);
		}, [channelStates]);

		return (
			<div className={styles.channelContainer}>
				<AvatarStack size={28} maxVisible={10}>
					{uniqueUserIds.map((uid) => (
						<StackUserAvatar key={uid} guild={guild} channel={channel} userId={uid} />
					))}
				</AvatarStack>
			</div>
		);
	},
);
