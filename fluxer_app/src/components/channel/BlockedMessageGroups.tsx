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

import {useLingui} from '@lingui/react/macro';
import React from 'react';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import {type ChannelStreamItem, ChannelStreamType} from '~/utils/MessageGroupingUtils';
import styles from './BlockedMessageGroups.module.css';
import {Divider} from './Divider';
import {MessageGroup} from './MessageGroup';

interface BlockedMessageGroupsProps {
	channel: ChannelRecord;
	messageGroups: Array<ChannelStreamItem>;
	onReveal: (messageId: string | null) => void;
	revealed: boolean;
	compact: boolean;
	messageGroupSpacing: number;
}

const arePropsEqual = (prevProps: BlockedMessageGroupsProps, nextProps: BlockedMessageGroupsProps): boolean => {
	if (prevProps.channel.id !== nextProps.channel.id) return false;
	if (prevProps.revealed !== nextProps.revealed) return false;
	if (prevProps.compact !== nextProps.compact) return false;
	if (prevProps.messageGroupSpacing !== nextProps.messageGroupSpacing) return false;
	if (prevProps.messageGroups.length !== nextProps.messageGroups.length) return false;

	for (let i = 0; i < prevProps.messageGroups.length; i++) {
		const prevGroup = prevProps.messageGroups[i];
		const nextGroup = nextProps.messageGroups[i];

		if (!nextGroup) return false;
		if (prevGroup.type !== nextGroup.type) return false;

		if (prevGroup.type === ChannelStreamType.MESSAGE) {
			const prevMessage = prevGroup.content as MessageRecord;
			const nextMessage = nextGroup.content as MessageRecord;

			if (prevMessage.id !== nextMessage.id) return false;
			if (prevMessage.editedTimestamp !== nextMessage.editedTimestamp) return false;
		}
	}

	return true;
};

export const BlockedMessageGroups = React.memo<BlockedMessageGroupsProps>((props) => {
	const {t} = useLingui();
	const {messageGroups, channel, compact, revealed, messageGroupSpacing, onReveal} = props;
	const containerRef = React.useRef<HTMLDivElement>(null);

	const handleClick = React.useCallback(() => {
		const container = containerRef.current;
		const scroller = container?.closest('[data-scroller]') as HTMLElement | null;

		if (scroller) {
			const wasAtBottom = scroller.scrollHeight - scroller.scrollTop - scroller.clientHeight < 1;

			if (revealed) {
				onReveal(null);
				if (wasAtBottom) {
					requestAnimationFrame(() => {
						scroller.scrollTop = scroller.scrollHeight;
					});
				}
			} else {
				const firstMessage = messageGroups.find((item) => item.type === ChannelStreamType.MESSAGE);
				if (firstMessage) {
					onReveal((firstMessage.content as MessageRecord).id);
					if (wasAtBottom) {
						requestAnimationFrame(() => {
							scroller.scrollTop = scroller.scrollHeight;
						});
					}
				}
			}
		} else {
			if (revealed) {
				onReveal(null);
			} else {
				const firstMessage = messageGroups.find((item) => item.type === ChannelStreamType.MESSAGE);
				if (firstMessage) {
					onReveal((firstMessage.content as MessageRecord).id);
				}
			}
		}
	}, [messageGroups, onReveal, revealed]);

	const totalMessageCount = React.useMemo(() => {
		return messageGroups.filter((item) => item.type === ChannelStreamType.MESSAGE).length;
	}, [messageGroups]);

	const messageNodes = React.useMemo(() => {
		if (!revealed) return null;

		const nodes: Array<React.ReactNode> = [];
		let currentGroupMessages: Array<MessageRecord> = [];
		let groupId: string | undefined;

		const flushGroup = () => {
			if (currentGroupMessages.length > 0) {
				nodes.push(
					<MessageGroup
						key={currentGroupMessages[0].id}
						messages={currentGroupMessages}
						channel={channel}
						messageDisplayCompact={compact}
						idPrefix="blocked-messages"
					/>,
				);
				currentGroupMessages = [];
				groupId = undefined;
			}
		};

		messageGroups.forEach((item, itemIndex) => {
			if (item.type === ChannelStreamType.DIVIDER) {
				flushGroup();
				nodes.push(
					<Divider
						key={item.unreadId || item.contentKey || `divider-${itemIndex}`}
						spacing={messageGroupSpacing}
						red={!!item.unreadId}
						id={item.unreadId ? 'new-messages-bar' : undefined}
					>
						{item.content as string}
					</Divider>,
				);
			} else if (item.type === ChannelStreamType.MESSAGE) {
				const message = item.content as MessageRecord;

				if (groupId !== item.groupId) {
					flushGroup();
					groupId = item.groupId;
				}

				currentGroupMessages.push(message);
			}
		});

		flushGroup();
		return nodes;
	}, [revealed, messageGroups, messageGroupSpacing, channel, compact]);

	return (
		<div ref={containerRef} className={styles.container}>
			<button type="button" className={styles.toggle} onClick={handleClick}>
				{totalMessageCount === 1 ? t`${totalMessageCount} Blocked Message` : t`${totalMessageCount} Blocked Messages`}
			</button>
			{revealed && (
				<div className={styles.content} data-blocked-messages>
					{messageNodes}
				</div>
			)}
		</div>
	);
}, arePropsEqual);
