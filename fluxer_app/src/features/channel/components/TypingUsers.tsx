// SPDX-License-Identifier: AGPL-3.0-or-later

import {Typing} from '@app/features/channel/components/ChannelTyping';
import styles from '@app/features/channel/components/TypingUsers.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import RollingTypingStore from '@app/features/typing/rolling/RollingTypingStore';
import {getRollingTypingAnnouncement, getRollingTypingText} from '@app/features/typing/rolling/RollingTypingText';
import {
	TYPING_ROLLING_MAX_NAMES,
	TYPING_ROLLING_OVERFLOW_SLACK_PX,
} from '@app/features/typing/rolling/TypingSendThrottle';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import type {I18n} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {type ReactNode, type RefObject, useLayoutEffect, useRef, useState} from 'react';

const AVATAR_THRESHOLD = 5;
const EMPTY_TYPING_USER_RECORDS: ReadonlyArray<User> = Object.freeze([]);

export const getTypingText = (i18n: I18n, typingUsers: ReadonlyArray<User>, channel: Channel): ReactNode =>
	getRollingTypingText(i18n, typingUsers, channel, false);

export const usePresentableTypingUsers = (channel: Channel): ReadonlyArray<User> => {
	const typingUserIds = RollingTypingStore.getTypingUserIds(channel.id);
	if (typingUserIds.length === 0) {
		return EMPTY_TYPING_USER_RECORDS;
	}
	const typingUsers: Array<User> = [];
	for (const userId of typingUserIds) {
		const user = Users.getUser(userId);
		if (user) {
			typingUsers.push(user);
		}
	}
	return typingUsers;
};

function getRenderedAvatarStackWidth(row: HTMLElement | null): number {
	if (row === null) {
		return 0;
	}
	for (const child of row.children) {
		if (child.classList.contains(messageStyles.typingAvatarContainer)) {
			return child.getBoundingClientRect().width;
		}
	}
	return 0;
}

interface TypingUsersProps {
	channel: Channel;
	withText?: boolean;
	showAvatars?: boolean;
	overflowContainerRef?: RefObject<HTMLElement | null>;
}

export const TypingUsers = observer(
	({channel, withText = true, showAvatars = true, overflowContainerRef}: TypingUsersProps) => {
		const {i18n} = useLingui();
		const typingUsers = usePresentableTypingUsers(channel);
		const rowRef = useRef<HTMLDivElement>(null);
		const measureRef = useRef<HTMLSpanElement>(null);
		const [overflowing, setOverflowing] = useState(false);
		const measuresOverflow = withText && typingUsers.length > 0 && typingUsers.length <= TYPING_ROLLING_MAX_NAMES;
		useLayoutEffect(() => {
			const container = overflowContainerRef?.current ?? null;
			const measure = measureRef.current;
			if (!measuresOverflow || container === null || measure === null) {
				return;
			}
			const evaluate = () => {
				const availableWidth = container.clientWidth - getRenderedAvatarStackWidth(rowRef.current);
				setOverflowing(measure.scrollWidth + TYPING_ROLLING_OVERFLOW_SLACK_PX > availableWidth);
			};
			evaluate();
			const resizeObserver = new ResizeObserver(evaluate);
			resizeObserver.observe(container);
			resizeObserver.observe(measure);
			return () => resizeObserver.disconnect();
		}, [measuresOverflow, overflowContainerRef]);
		if (typingUsers.length === 0) {
			return null;
		}
		return (
			<div
				className={`${messageStyles.typingContainer} ${messageStyles.typingCluster} ${messageStyles.typingClusterComposerStatus}`}
				data-flx="channel.typing-users.div"
			>
				<div ref={rowRef} className={styles.composerStatus} data-flx="channel.typing-users.div--2">
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
							<span aria-hidden={true} className={messageStyles.typingText} data-flx="channel.typing-users.span">
								{getRollingTypingText(i18n, typingUsers, channel, measuresOverflow && overflowing)}
							</span>
							{measuresOverflow && (
								<span
									ref={measureRef}
									aria-hidden={true}
									className={`${messageStyles.typingText} ${styles.measure}`}
									data-flx="channel.typing-users.measure"
								>
									{getRollingTypingText(i18n, typingUsers, channel, false)}
								</span>
							)}
						</>
					)}
				</div>
			</div>
		);
	},
);

export const TypingAnnouncer = observer(({channel}: {channel: Channel}) => {
	const {i18n} = useLingui();
	const typingUsers = usePresentableTypingUsers(channel);
	return (
		<span className={styles.srOnly} aria-live="polite" aria-atomic={true} data-flx="channel.typing-users.announcer">
			{getRollingTypingAnnouncement(i18n, typingUsers, channel)}
		</span>
	);
});
