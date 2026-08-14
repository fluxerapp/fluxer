// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import {Typing} from '@app/features/channel/components/ChannelTyping';
import styles from '@app/features/channel/components/TypingUsers.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Relationships from '@app/features/relationship/state/Relationships';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import TypingIndicator from '@app/features/typing/state/TypingIndicator';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const SEVERAL_PEOPLE_ARE_TYPING_DESCRIPTOR = msg({
	message: 'Several people are typing...',
	comment: 'Label in the channel and chat typing users.',
});
const A_HANDFUL_OF_KEYBOARD_WARRIORS_ARE_ASSEMBLING_DESCRIPTOR = msg({
	message: 'A handful of keyboard warriors are assembling...',
	comment: 'Description text in the channel and chat typing users.',
});
const A_SYMPHONY_OF_CLACKING_KEYS_IS_UNDERWAY_DESCRIPTOR = msg({
	message: 'A symphony of clacking keys is underway...',
	comment: 'Description text in the channel and chat typing users.',
});
const IT_S_A_FULL_BLOWN_TYPING_FIESTA_IN_DESCRIPTOR = msg({
	message: "It's a full-blown typing fiesta in here",
	comment: 'Label in the channel and chat typing users.',
});
const WHOA_IT_S_A_TYPING_APOCALYPSE_DESCRIPTOR = msg({
	message: "Whoa, it's a typing apocalypse",
	comment: 'Label in the channel and chat typing users.',
});
const SEVERAL_PEOPLE_DESCRIPTOR = SEVERAL_PEOPLE_ARE_TYPING_DESCRIPTOR;
const HANDFUL_DESCRIPTOR = A_HANDFUL_OF_KEYBOARD_WARRIORS_ARE_ASSEMBLING_DESCRIPTOR;
const SYMPHONY_DESCRIPTOR = A_SYMPHONY_OF_CLACKING_KEYS_IS_UNDERWAY_DESCRIPTOR;
const FIESTA_DESCRIPTOR = IT_S_A_FULL_BLOWN_TYPING_FIESTA_IN_DESCRIPTOR;
const APOCALYPSE_DESCRIPTOR = WHOA_IT_S_A_TYPING_APOCALYPSE_DESCRIPTOR;
const getDisplayName = (user: User, guildId?: string | null) => NicknameUtils.getNickname(user, guildId ?? undefined);
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
	if (typingUsers.length === 4) {
		return i18n._(SEVERAL_PEOPLE_DESCRIPTOR);
	}
	if (typingUsers.length > 4 && typingUsers.length < 10) {
		return i18n._(HANDFUL_DESCRIPTOR);
	}
	if (typingUsers.length > 9 && typingUsers.length < 15) {
		return i18n._(SYMPHONY_DESCRIPTOR);
	}
	if (typingUsers.length > 14 && typingUsers.length < 20) {
		return i18n._(FIESTA_DESCRIPTOR);
	}
	return i18n._(APOCALYPSE_DESCRIPTOR);
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
