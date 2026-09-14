// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import {Typing} from '@app/features/channel/components/ChannelTyping';
import styles from '@app/features/channel/components/TypingUsers.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Relationships from '@app/features/relationship/state/Relationships';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import TypingIndicator from '@app/features/typing/legacy/LegacyTypingIndicator';
import {getTypingTierText} from '@app/features/typing/utils/TypingTierText';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {I18n} from '@lingui/core';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const getDisplayName = (user: User, guildId?: string | null) => NicknameUtils.getNickname(user, guildId ?? null);
export const getTypingText = (i18n: I18n, typingUsers: ReadonlyArray<User>, channel: Channel) => {
	const [a, b, c] = typingUsers.map((user) => {
		const member = GuildMembers.getMember(channel.guildId ?? '', user.id);
		return (
			<span
				key={user.id}
				className={styles.username}
				style={{color: member?.getColorString()}}
				data-flx="channel.typing-users.get-typing-text.username"
			>
				{getDisplayName(user, channel.guildId)}
			</span>
		);
	});
	if (typingUsers.length === 1) {
		return <Trans>{a} is typing...</Trans>;
	}
	if (typingUsers.length === 2) {
		return (
			<Trans>
				{a} and {b} are typing...
			</Trans>
		);
	}
	if (typingUsers.length === 3) {
		return (
			<Trans>
				{a}, {b} and {c} are typing...
			</Trans>
		);
	}
	return getTypingTierText(i18n, typingUsers.length);
};
const EMPTY_TYPING_USER_RECORDS: ReadonlyArray<User> = Object.freeze([]);
export const usePresentableTypingUsers = (channel: Channel): ReadonlyArray<User> => {
	const typingUserIds = TypingIndicator.getTypingUsers(channel.id);
	if (typingUserIds.length === 0) return EMPTY_TYPING_USER_RECORDS;
	const currentUserId = Authentication.currentUserId;
	const showSelf = DeveloperOptions.showMyselfTyping;
	const result: Array<User> = [];
	for (const userId of typingUserIds) {
		if (!showSelf && userId === currentUserId) continue;
		if (Relationships.isBlocked(userId)) continue;
		const user = Users.getUser(userId);
		if (user) result.push(user);
	}
	if (result.length === 0) return EMPTY_TYPING_USER_RECORDS;
	return result;
};
const AVATAR_THRESHOLD = 5;
export const TypingUsers = observer(
	({channel, withText = true, showAvatars = true}: {channel: Channel; withText?: boolean; showAvatars?: boolean}) => {
		const {i18n} = useLingui();
		const typingUsers = usePresentableTypingUsers(channel);
		if (typingUsers.length === 0) {
			return null;
		}
		return (
			<div
				className={`${messageStyles.typingContainer} ${messageStyles.typingCluster} ${messageStyles.typingClusterComposerStatus}`}
				data-flx="channel.typing-users.div"
			>
				<div className={styles.composerStatus} data-flx="channel.typing-users.div--2">
					<div className={messageStyles.typingIndicator} data-flx="channel.typing-users.div--3">
						<Typing
							className={styles.typing}
							size={20}
							style={{
								height: 'var(--typing-indicator-animation-size)',
								width: 'var(--typing-indicator-animation-size)',
							}}
							data-flx="channel.typing-users.typing"
						/>
					</div>
					{withText && (
						<>
							{showAvatars && (
								<AvatarStack
									size={12}
									maxVisible={AVATAR_THRESHOLD}
									className={messageStyles.typingAvatarContainer}
									users={typingUsers}
									guildId={channel.guildId}
									channelId={channel.id}
									data-flx="channel.typing-users.avatar-stack"
								/>
							)}
							<span
								aria-atomic={true}
								aria-live="polite"
								className={messageStyles.typingText}
								data-flx="channel.typing-users.span"
							>
								{getTypingText(i18n, typingUsers, channel)}
							</span>
						</>
					)}
				</div>
			</div>
		);
	},
);
