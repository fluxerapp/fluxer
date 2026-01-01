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

import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import UserStore from '~/stores/UserStore';
import MediaEngineStore from '~/stores/voice/MediaEngineFacade';
import {AvatarWithPresence} from './AvatarWithPresence';
import styles from './StackUserAvatar.module.css';

interface StackUserAvatarProps {
	guild: GuildRecord;
	channel: ChannelRecord;
	userId: string;
	size?: number;
	className?: string;
}

export const StackUserAvatar = observer(({guild, channel, userId, size = 28, className}: StackUserAvatarProps) => {
	const channelStates = MediaEngineStore.getAllVoiceStatesInChannel(guild.id, channel.id);
	const user = UserStore.getUser(userId);
	if (!user) return null;

	let speaking = false;
	for (const state of Object.values(channelStates)) {
		if (state.user_id !== userId) continue;
		const connectionId = state.connection_id ?? '';
		const participant = MediaEngineStore.getParticipantByUserIdAndConnectionId(userId, connectionId);
		const selfMuted = state.self_mute ?? (participant ? !participant.isMicrophoneEnabled : false);
		const guildMuted = state.mute ?? false;

		speaking ||= !!(participant?.isSpeaking && !selfMuted && !guildMuted);
	}

	return (
		<AvatarWithPresence
			user={user}
			size={size}
			speaking={speaking}
			className={clsx(styles.container, className)}
			title={user.username}
		/>
	);
});
