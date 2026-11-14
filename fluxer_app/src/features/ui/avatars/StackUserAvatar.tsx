// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {Guild} from '@app/features/guild/models/Guild';
import {AvatarWithPresence} from '@app/features/ui/avatars/AvatarWithPresence';
import styles from '@app/features/ui/avatars/StackUserAvatar.module.css';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {resolveVoiceParticipantSpeaking} from '@app/features/voice/components/VoiceParticipantDisplayState';
import VoiceRuntime from '@app/features/voice/engine/VoiceRuntime';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

interface StackUserAvatarProps {
	guild: Guild;
	channel: Channel;
	userId: string;
	size?: number;
	className?: string;
}

export const StackUserAvatar = observer(({guild, channel, userId, size = 28, className}: StackUserAvatarProps) => {
	const channelStates = VoiceRuntime.getAllVoiceStatesInChannel(guild.id, channel.id);
	const user = Users.getUser(userId);
	if (!user) return null;
	let speaking = false;
	for (const state of Object.values(channelStates)) {
		if (state.user_id !== userId) continue;
		const connectionId = state.connection_id ?? '';
		const participant = VoiceRuntime.getParticipantByUserIdAndConnectionId(userId, connectionId);
		speaking ||= resolveVoiceParticipantSpeaking({
			participant,
			voiceState: state,
			isLocalConnection: false,
			localSelfMute: false,
			permissionMuted: false,
		});
	}
	return (
		<AvatarWithPresence
			user={user}
			size={size}
			speaking={speaking}
			className={clsx(styles.container, className)}
			ariaLabel={NicknameUtils.getNickname(user, guild.id, channel.id)}
			data-flx="ui.avatars.stack-user-avatar.container"
		/>
	);
});
