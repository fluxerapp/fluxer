// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ThreadPreviewCard.module.css';
import * as ThreadCommands from '@app/features/channel/commands/ThreadCommands';
import {openThreadContextMenu} from '@app/features/channel/components/ThreadContextMenu';
import Threads from '@app/features/channel/state/Threads';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {getFormattedShortDate} from '@app/features/user/utils/DateFormatting';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretRightIcon, ClockIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';
import type React from 'react';

const MESSAGES_DESCRIPTOR = msg({
	message: '· {count, plural, one {# message} other {# messages}}',
	comment: 'Message count shown on thread preview card.',
});
const NO_MESSAGES_DESCRIPTOR = msg({
	message: 'There are no recent messages in this thread.',
	comment: 'Placeholder shown on thread preview card when there are no messages.',
});
const CLOSES_IN_DESCRIPTOR = msg({
	message: 'Closes in {days, plural, one {# day} other {# days}} · {date}',
	comment: 'Expiry row on thread preview card when thread is still open.',
});
const EXPIRED_DESCRIPTOR = msg({
	message: 'Expired · {date}',
	comment: 'Expiry row on thread preview card when the thread has passed its expiry date.',
});

interface ThreadPreviewCardProps {
	threadId: string;
	threadName: string;
	guildId?: string;
	parentChannelId: string;
}

export const ThreadPreviewCard = observer(({threadId, threadName, guildId, parentChannelId}: ThreadPreviewCardProps) => {
	const {i18n} = useLingui();
	const thread = Threads.getThread(threadId);
	const isJoined = Threads.isJoined(threadId);

	const name = thread?.name ?? threadName;
	const preview = thread?.preview;
	const isOpen = thread ? thread.isOpen() : false;
	const messageCount = thread?.messageCount ?? 0;
	const expiresAt = thread?.threadExpiresAt ?? null;

	const handleClick = useCallback(async () => {
		if (!isJoined) {
			await ThreadCommands.join(parentChannelId, threadId);
		}
		if (guildId) {
			NavigationCommands.selectThread(guildId, parentChannelId, threadId);
		}
	}, [threadId, parentChannelId, guildId, isJoined]);

	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			if (thread && guildId) {
				openThreadContextMenu(event, {thread, guildId, i18n});
			}
		},
		[thread, guildId, i18n],
	);

	const avatarUrl = preview?.lastMessageAuthorId
		? AvatarUtils.getUserAvatarURL({
				id: preview.lastMessageAuthorId,
				avatar: preview.lastMessageAuthorAvatar ?? null,
			})
		: null;

	const expiryLabel = (() => {
		if (!expiresAt) return null;
		const now = Date.now();
		const date = getFormattedShortDate(expiresAt);
		if (expiresAt.getTime() <= now) {
			return {expired: true, text: i18n._(EXPIRED_DESCRIPTOR, {date})};
		}
		const days = Math.ceil((expiresAt.getTime() - now) / 86_400_000);
		return {expired: false, text: i18n._(CLOSES_IN_DESCRIPTOR, {days, date})};
	})();

	return (
		<div className={styles.wrap} onContextMenu={handleContextMenu} data-flx="channel.thread-preview-card.wrap">
			<svg
				className={styles.branchArm}
				viewBox="0 0 56 52"
				preserveAspectRatio="none"
				fill="none"
				aria-hidden="true"
				data-flx="channel.thread-preview-card.branch-arm"
			>
				<path d="M6 0 V38 Q6 48 16 48 H56" stroke="currentColor" strokeWidth="2" fill="none" vectorEffect="non-scaling-stroke" />
			</svg>
			<div className={styles.cardColumn} data-flx="channel.thread-preview-card.card-column">
				<div
					role="button"
					tabIndex={0}
					className={clsx(styles.box, isOpen ? styles.boxActive : undefined)}
					onClick={handleClick}
					onKeyDown={(e: React.KeyboardEvent) => {
						if (e.key === 'Enter' || e.key === ' ') {
							e.preventDefault();
							void handleClick();
						}
					}}
					data-flx="channel.thread-preview-card.box.click"
				>
					<div className={styles.header} data-flx="channel.thread-preview-card.header">
						<div className={styles.titleRow} data-flx="channel.thread-preview-card.title-row">
							<span
								className={clsx(styles.threadName, !isOpen ? styles.threadNameClosed : undefined)}
								data-flx="channel.thread-preview-card.thread-name"
							>
								{name}
							</span>
							{messageCount > 0 && (
								<span className={styles.messageCount} data-flx="channel.thread-preview-card.message-count">
									{i18n._(MESSAGES_DESCRIPTOR, {count: messageCount})}
								</span>
							)}
						</div>
						<CaretRightIcon size={14} className={styles.chevron} aria-hidden="true" data-flx="channel.thread-preview-card.chevron" />
					</div>
					<div className={styles.preview} data-flx="channel.thread-preview-card.preview">
						{avatarUrl ? (
							<img
								src={avatarUrl}
								alt=""
								className={styles.miniAvatar}
								data-flx="channel.thread-preview-card.mini-avatar"
							/>
						) : (
							<div className={styles.miniAvatarPlaceholder} data-flx="channel.thread-preview-card.mini-avatar-placeholder" />
						)}
						<div className={styles.previewTextWrap} data-flx="channel.thread-preview-card.preview-text-wrap">
							{preview?.lastMessageAuthorUsername && (
								<span className={styles.previewSender} data-flx="channel.thread-preview-card.preview-sender">
									{preview.lastMessageAuthorUsername}
								</span>
							)}
							<span className={styles.previewMsg} data-flx="channel.thread-preview-card.preview-msg">
								{preview?.lastMessagePreview ?? i18n._(NO_MESSAGES_DESCRIPTOR)}
							</span>
						</div>
					</div>
				</div>
				{expiryLabel && (
					<div
						className={clsx(styles.expiryRow, expiryLabel.expired ? styles.expiryRowExpired : undefined)}
						data-flx="channel.thread-preview-card.expiry-row"
					>
						{expiryLabel.expired
							? <WarningCircleIcon size={12} aria-hidden="true" />
							: <ClockIcon size={12} aria-hidden="true" />
						}
						<span>{expiryLabel.text}</span>
					</div>
				)}
			</div>
		</div>
	);
});
