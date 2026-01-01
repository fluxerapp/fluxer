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

import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Fragment} from 'react';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import {Message} from './Message';
import {UnreadDividerSlot} from './UnreadDividerSlot';

interface MessageGroupProps {
	messages: Array<MessageRecord>;
	channel: ChannelRecord;
	onEdit?: (targetNode: HTMLElement) => void;
	jumpSequenceId?: number;
	highlightedMessageId?: string | null;
	messageDisplayCompact?: boolean;
	flashKey?: number;
	getUnreadDividerVisibility?: (messageId: string, position: 'before' | 'after') => boolean;
	idPrefix?: string;
}

export const MessageGroup: React.FC<MessageGroupProps> = observer((props) => {
	const {
		messages,
		channel,
		onEdit,
		jumpSequenceId,
		highlightedMessageId,
		messageDisplayCompact = false,
		getUnreadDividerVisibility,
		idPrefix,
	} = props;

	const groupId = messages[0]?.id;

	return (
		<div data-jump-sequence-id={jumpSequenceId} data-group-id={groupId} role="group" aria-label="Message group">
			{messages.map((message, index) => {
				const prevMessage = messages[index - 1];
				const isGroupStart = index === 0;

				return (
					<Fragment key={message.id}>
						{getUnreadDividerVisibility && (
							<UnreadDividerSlot beforeId={message.id} visible={getUnreadDividerVisibility(message.id, 'before')} />
						)}

						<div data-message-index={index} data-message-id={message.id} data-is-group-start={isGroupStart}>
							<Message
								channel={channel}
								message={message}
								prevMessage={prevMessage}
								onEdit={onEdit}
								shouldGroup={!isGroupStart}
								isJumpTarget={highlightedMessageId === message.id}
								compact={messageDisplayCompact}
								idPrefix={idPrefix}
							/>
						</div>
					</Fragment>
				);
			})}
		</div>
	);
});
