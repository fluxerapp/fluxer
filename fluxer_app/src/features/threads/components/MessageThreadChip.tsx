// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {buildMessageNotificationBody} from '@app/features/notification/utils/MessageNotificationPreview';
import {openThread} from '@app/features/threads/commands/ThreadNavigation';
import styles from '@app/features/threads/components/MessageThreadChip.module.css';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {useLingui} from '@lingui/react/macro';
import {CaretRightIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

export const MessageThreadChip = observer(({message}: {message: Message}) => {
	const {i18n} = useLingui();
	if (!ThreadGuilds.anyActive) return null;
	const thread = ChannelThreads.getThread(message.id);
	if (!thread || thread.parentId !== message.channelId || !ThreadGuilds.isActive(thread.guildId)) return null;
	const lastMessage = thread.lastMessageId ? Messages.getMessage(thread.id, thread.lastMessageId) : undefined;
	const preview = lastMessage
		? `${NicknameUtils.getNickname(lastMessage.author, thread.guildId, thread.id)}: ${buildMessageNotificationBody(lastMessage, i18n)}`
		: thread.messageCount === 0
			? i18n._(D.NO_RECENT_MESSAGES_DESCRIPTOR)
			: null;
	return (
		<FocusRing offset={-2} data-flx="threads.message-thread-chip.focus-ring">
			<button
				type="button"
				className={styles.chip}
				onClick={() => openThread(thread)}
				data-flx="threads.message-thread-chip.chip.open"
			>
				<span className={styles.row} data-flx="threads.message-thread-chip.row">
					<span className={styles.name} data-flx="threads.message-thread-chip.name">
						{thread.name}
					</span>
					<span className={styles.count} data-flx="threads.message-thread-chip.count">
						<span data-flx="threads.message-thread-chip.count-label">
							{i18n._(D.MESSAGE_COUNT_DESCRIPTOR, {count: thread.messageCount})}
						</span>
						<CaretRightIcon size={12} weight="bold" data-flx="threads.message-thread-chip.caret" />
					</span>
				</span>
				{preview && (
					<span className={styles.preview} data-flx="threads.message-thread-chip.preview">
						{preview}
					</span>
				)}
			</button>
		</FocusRing>
	);
});
