// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/CollapsedCategoryVoiceParticipants.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Guild} from '@app/features/guild/models/Guild';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import {StackUserAvatar} from '@app/features/ui/avatars/StackUserAvatar';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {
	createVoiceParticipantSortSnapshot,
	sortVoiceParticipantItemsWithSnapshot,
} from '@app/features/voice/components/VoiceParticipantSortUtils';
import VoiceRuntime from '@app/features/voice/engine/VoiceRuntime';
import {useVoiceGatewayStateVersion} from '@app/features/voice/engine/v2/VoiceEngineV2AppVoiceStateAdapter';
import {SpeakerHighIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useMemo, useRef} from 'react';

export const CollapsedCategoryVoiceParticipants = observer(
	({guild, voiceChannels}: {guild: Guild; voiceChannels: Array<Channel>}) => {
		useVoiceGatewayStateVersion();
		const allVoiceStates = VoiceRuntime.getAllVoiceStates();
		const userSortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
		const voiceUserSummary = useMemo(() => {
			const userIds: Array<string> = [];
			const seenUserIds = new Set<string>();
			const firstChannelByUserId = new Map<string, Channel>();
			for (const channel of voiceChannels) {
				const states = allVoiceStates[guild.id]?.[channel.id];
				if (!states) continue;
				for (const connectionId in states) {
					const voiceState = states[connectionId];
					if (!voiceState) continue;
					if (!firstChannelByUserId.has(voiceState.user_id)) {
						firstChannelByUserId.set(voiceState.user_id, channel);
					}
					if (seenUserIds.has(voiceState.user_id)) continue;
					seenUserIds.add(voiceState.user_id);
					userIds.push(voiceState.user_id);
				}
			}
			const sortedUserIds = sortVoiceParticipantItemsWithSnapshot(userIds, {
				snapshot: userSortSnapshotRef.current,
				getParticipantKey: (userId) => userId,
				getUserId: (userId) => userId,
				guildId: guild.id,
			});
			return {firstChannelByUserId, userIds: sortedUserIds};
		}, [allVoiceStates, guild.id, voiceChannels]);
		const users = useMemo(() => {
			const nextUsers: Array<User> = [];
			for (const userId of voiceUserSummary.userIds) {
				const user = Users.getUser(userId);
				if (user) {
					nextUsers.push(user);
				}
			}
			return nextUsers;
		}, [voiceUserSummary.userIds, Users.usersList]);
		if (voiceUserSummary.userIds.length === 0) return null;
		return (
			<div className={styles.container} data-flx="app.collapsed-category-voice-participants.container">
				<SpeakerHighIcon className={styles.icon} data-flx="app.collapsed-category-voice-participants.icon" />
				<AvatarStack
					size={28}
					maxVisible={5}
					users={users}
					guildId={guild.id}
					renderAvatar={(user, size) => {
						const ch = voiceUserSummary.firstChannelByUserId.get(user.id);
						return ch ? (
							<StackUserAvatar
								guild={guild}
								channel={ch}
								userId={user.id}
								size={size}
								data-flx="app.collapsed-category-voice-participants.stack-user-avatar"
							/>
						) : null;
					}}
					data-flx="app.collapsed-category-voice-participants.avatar-stack"
				/>
			</div>
		);
	},
);
export const CollapsedChannelAvatarStack = observer(({guild, channel}: {guild: Guild; channel: Channel}) => {
	useVoiceGatewayStateVersion();
	const channelStates = VoiceRuntime.getAllVoiceStatesInChannel(guild.id, channel.id);
	const userSortSnapshotRef = useRef(createVoiceParticipantSortSnapshot());
	const uniqueUserIds = useMemo(() => {
		const userIds: Array<string> = [];
		const seenUserIds = new Set<string>();
		for (const connectionId in channelStates) {
			const voiceState = channelStates[connectionId];
			if (!voiceState) continue;
			if (seenUserIds.has(voiceState.user_id)) continue;
			seenUserIds.add(voiceState.user_id);
			userIds.push(voiceState.user_id);
		}
		return sortVoiceParticipantItemsWithSnapshot(userIds, {
			snapshot: userSortSnapshotRef.current,
			getParticipantKey: (userId) => userId,
			getUserId: (userId) => userId,
			guildId: guild.id,
			channelId: channel.id,
		});
	}, [channel.id, channelStates, guild.id]);
	const users = useMemo(() => {
		const nextUsers: Array<User> = [];
		for (const userId of uniqueUserIds) {
			const user = Users.getUser(userId);
			if (user) {
				nextUsers.push(user);
			}
		}
		return nextUsers;
	}, [uniqueUserIds, Users.usersList]);
	return (
		<div
			className={styles.channelContainer}
			data-flx="app.collapsed-category-voice-participants.collapsed-channel-avatar-stack.channel-container"
		>
			<AvatarStack
				size={28}
				maxVisible={5}
				users={users}
				guildId={guild.id}
				channelId={channel.id}
				renderAvatar={(user, size) => (
					<StackUserAvatar
						guild={guild}
						channel={channel}
						userId={user.id}
						size={size}
						data-flx="app.collapsed-category-voice-participants.collapsed-channel-avatar-stack.stack-user-avatar"
					/>
				)}
				data-flx="app.collapsed-category-voice-participants.collapsed-channel-avatar-stack.avatar-stack"
			/>
		</div>
	);
});
