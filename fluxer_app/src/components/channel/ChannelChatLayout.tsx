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
import type React from 'react';
import {SlowmodeIndicator} from '~/components/channel/SlowmodeIndicator';
import {useSlowmode} from '~/hooks/useSlowmode';
import type {ChannelRecord} from '~/records/ChannelRecord';
import MessageEditMobileStore from '~/stores/MessageEditMobileStore';
import MessageReplyStore from '~/stores/MessageReplyStore';
import MessageStore from '~/stores/MessageStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import styles from './ChannelChatLayout.module.css';
import {TypingUsers} from './TypingUsers';

interface ChannelChatLayoutProps {
	channel: ChannelRecord;
	messages: React.ReactNode;
	textarea: React.ReactNode;
}

export const ChannelChatLayout = observer(({channel, messages, textarea}: ChannelChatLayoutProps) => {
	const {isSlowmodeActive, slowmodeRemaining} = useSlowmode(channel);
	const hasSlowmodeIndicator = isSlowmodeActive && slowmodeRemaining > 0;

	const replyingMessage = MessageReplyStore.getReplyingMessage(channel.id);
	const referencedMessage = replyingMessage ? MessageStore.getMessage(channel.id, replyingMessage.messageId) : null;
	const editingMobileMessageId = MessageEditMobileStore.getEditingMobileMessageId(channel.id);
	const editingMessage = editingMobileMessageId ? MessageStore.getMessage(channel.id, editingMobileMessageId) : null;
	const hasTopBar = Boolean(referencedMessage || (editingMessage && MobileLayoutStore.enabled));

	return (
		<div className={styles.container}>
			<div className={styles.messagesArea}>{messages}</div>
			<div className={clsx(styles.typingArea, hasTopBar && styles.typingAreaWithTopBar)}>
				<div className={styles.typingContent}>
					<div className={styles.typingLeft}>
						<TypingUsers channel={channel} />
					</div>
					{hasSlowmodeIndicator && (
						<div className={styles.typingRight}>
							<SlowmodeIndicator slowmodeRemaining={slowmodeRemaining} />
						</div>
					)}
				</div>
			</div>
			<div className={styles.textareaArea}>{textarea}</div>
		</div>
	);
});
