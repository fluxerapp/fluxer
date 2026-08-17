// SPDX-License-Identifier: AGPL-3.0-or-later

import {BlockedMessageGroups} from '@app/features/channel/components/BlockedMessageGroups';
import {Divider} from '@app/features/channel/components/ChannelDivider';
import {getUnreadDividerBeforeMessageId} from '@app/features/channel/components/ChannelMessageStreamUtils';
import styles from '@app/features/channel/components/ChannelMessages.module.css';
import {MessageGroup} from '@app/features/channel/components/MessageGroup';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {ChannelMessages} from '@app/features/messaging/state/ChannelMessages';
import {type ChannelStreamItem, ChannelStreamType} from '@app/features/messaging/utils/MessageGroupingUtils';
import {IS_DEV} from '@app/features/platform/types/Env';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type React from 'react';

const logger = new Logger('ChannelMessageStream');
const isSystemMessage = (message: Message | undefined): boolean => {
	if (!message) return false;
	return message.type !== MessageTypes.DEFAULT && message.type !== MessageTypes.REPLY;
};

type MessageGroupKind = 'system' | 'regular';

const getMessageGroupKind = (message: Message | undefined): MessageGroupKind => {
	return isSystemMessage(message) ? 'system' : 'regular';
};

interface RenderChannelStreamProps {
	channelStream: Array<ChannelStreamItem>;
	messages: ChannelMessages;
	channel: Channel;
	highlightedMessageId: string | null;
	messageDisplayCompact: boolean;
	messageGroupSpacing: number;
	revealedMessageId: string | null;
	onMessageEdit?: (target: HTMLElement) => void;
	onReveal?: (messageId: string | null) => void;
	messageRowClassName?: string;
	messageActionsClassName?: string;
	renderMessageActions?: (message: Message) => React.ReactNode;
	suppressMessageActions?: boolean;
	previewContext?: keyof typeof MessagePreviewContext;
	dateDividerClassName?: string;
	suppressUnreadIndicator?: boolean;
	getMessageHeadingActivate?: (message: Message) => (() => void) | undefined;
}

export function renderChannelStream(props: RenderChannelStreamProps): Array<React.ReactNode> {
	const {
		channelStream,
		channel,
		highlightedMessageId,
		messageDisplayCompact,
		messageGroupSpacing,
		revealedMessageId,
		onMessageEdit,
		onReveal,
		messageRowClassName,
		messageActionsClassName,
		renderMessageActions,
		suppressMessageActions,
		previewContext,
		dateDividerClassName,
		suppressUnreadIndicator,
		getMessageHeadingActivate,
	} = props;
	const nodes: Array<React.ReactNode> = [];
	const seenKeys = IS_DEV ? new Map<string, {type: string; index: number; detail: Record<string, unknown>}>() : null;
	const registerKey = (key: string | undefined, type: string, index: number, detail: Record<string, unknown> = {}) => {
		if (!IS_DEV || !key || !seenKeys) return;
		const existing = seenKeys.get(key);
		if (existing) {
			logger.warn('Duplicate channel stream key detected', {
				key,
				existing,
				next: {type, index, detail},
			});
			return;
		}
		seenKeys.set(key, {type, index, detail});
	};
	let pendingMessages: Array<Message> = [];
	let pendingStreamItems: Array<ChannelStreamItem> = [];
	let pendingGroupId: string | undefined;
	let pendingFlashKey: number | undefined;
	let lastRenderedGroupKind: MessageGroupKind | null = null;
	let spacerCounter = 0;
	let currentIndex = -1;
	const pushSpacerIfNeeded = (nextKind: MessageGroupKind, keyBase: string, nextMessageHasUnreadDivider = false) => {
		if (messageGroupSpacing <= 0 || lastRenderedGroupKind == null) return;
		if (nextMessageHasUnreadDivider) return;
		const bothSystem = lastRenderedGroupKind === 'system' && nextKind === 'system';
		const spacerClass = bothSystem ? styles.groupSpacerHalf : styles.groupSpacer;
		nodes.push(
			<div
				key={`group-spacer-${keyBase}-${spacerCounter++}`}
				className={spacerClass}
				aria-hidden="true"
				data-flx="channel.channel-message-stream.push-spacer-if-needed.div"
			/>,
		);
	};
	const flushPendingGroup = () => {
		if (pendingMessages.length === 0) return;
		const groupKey = pendingGroupId ?? pendingMessages[0].id;
		registerKey(groupKey, 'MessageGroup', currentIndex, {
			groupId: pendingGroupId ?? null,
			firstMessageId: pendingMessages[0]?.id ?? null,
			lastMessageId: pendingMessages[pendingMessages.length - 1]?.id ?? null,
			count: pendingMessages.length,
		});
		const groupKind = getMessageGroupKind(pendingMessages[0]);
		const unreadDividerBeforeMessageId = getUnreadDividerBeforeMessageId(pendingStreamItems, suppressUnreadIndicator);
		const firstMessageHasUnreadDivider = unreadDividerBeforeMessageId === pendingMessages[0].id;
		pushSpacerIfNeeded(groupKind, groupKey, firstMessageHasUnreadDivider);
		const getUnreadDividerVisibility = (messageId: string, position: 'before' | 'after') => {
			return position === 'before' && unreadDividerBeforeMessageId === messageId;
		};
		nodes.push(
			<MessageGroup
				key={groupKey}
				messages={pendingMessages}
				channel={channel}
				onEdit={onMessageEdit}
				highlightedMessageId={highlightedMessageId}
				messageDisplayCompact={messageDisplayCompact}
				flashKey={pendingFlashKey}
				getUnreadDividerVisibility={getUnreadDividerVisibility}
				idPrefix="chat-messages"
				messageRowClassName={messageRowClassName}
				messageActionsClassName={messageActionsClassName}
				renderMessageActions={renderMessageActions}
				suppressMessageActions={suppressMessageActions}
				previewContext={previewContext}
				getMessageHeadingActivate={getMessageHeadingActivate}
				data-flx="channel.channel-message-stream.flush-pending-group.message-group"
			/>,
		);
		lastRenderedGroupKind = groupKind;
		pendingMessages = [];
		pendingStreamItems = [];
		pendingGroupId = undefined;
		pendingFlashKey = undefined;
	};
	for (let i = 0; i < channelStream.length; i++) {
		const item = channelStream[i];
		currentIndex = i;
		if (item.type !== ChannelStreamType.MESSAGE) {
			flushPendingGroup();
			if (item.type === ChannelStreamType.DIVIDER) {
				const isUnread = item.unreadId != null && !suppressUnreadIndicator;
				const isDateDivider = !!item.content;
				const dividerSpacing = isDateDivider ? 16 : 0;
				const dividerKey = item.contentKey || `divider-${i}`;
				registerKey(dividerKey, 'Divider', i, {
					contentKey: item.contentKey ?? null,
					unreadId: item.unreadId ?? null,
				});
				nodes.push(
					<Divider
						key={dividerKey}
						spacing={dividerSpacing}
						red={isUnread}
						isDate={isDateDivider}
						id={isUnread ? 'new-messages-bar' : undefined}
						className={dateDividerClassName}
						data-flx="channel.channel-message-stream.render-channel-stream.divider"
					>
						{item.content as string}
					</Divider>,
				);
				lastRenderedGroupKind = null;
				continue;
			}
			if (
				item.type === ChannelStreamType.MESSAGE_GROUP_BLOCKED ||
				item.type === ChannelStreamType.MESSAGE_GROUP_SPAMMER
			) {
				const variant = item.type === ChannelStreamType.MESSAGE_GROUP_SPAMMER ? 'spammer' : 'blocked';
				registerKey(item.key, 'BlockedMessageGroups', i, {
					groupId: item.key ?? null,
					itemCount: Array.isArray(item.content) ? item.content.length : 0,
					revealed: item.key === revealedMessageId,
					variant,
				});
				pushSpacerIfNeeded('regular', item.key ?? `${variant}-${i}`);
				nodes.push(
					<BlockedMessageGroups
						key={item.key}
						revealed={item.key === revealedMessageId}
						messageGroups={item.content as Array<ChannelStreamItem>}
						onReveal={onReveal ?? (() => {})}
						compact={messageDisplayCompact}
						channel={channel}
						messageGroupSpacing={messageGroupSpacing}
						variant={variant}
						data-flx="channel.channel-message-stream.render-channel-stream.blocked-message-groups"
					/>,
				);
				lastRenderedGroupKind = 'regular';
				continue;
			}
			continue;
		}
		const message = item.content as Message;
		const itemGroupId = item.groupId ?? message.id;
		if (pendingGroupId && pendingGroupId !== itemGroupId) {
			flushPendingGroup();
		}
		if (!pendingGroupId) {
			pendingGroupId = itemGroupId;
		}
		pendingMessages.push(message);
		pendingStreamItems.push(item);
		if (item.flashKey != null) {
			pendingFlashKey = item.flashKey;
		}
	}
	flushPendingGroup();
	return nodes;
}
