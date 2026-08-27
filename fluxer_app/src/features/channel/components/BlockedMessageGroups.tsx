// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/BlockedMessageGroups.module.css';
import {Divider} from '@app/features/channel/components/ChannelDivider';
import {
	MessageGroup,
	type MessageGroupProps,
	type MessageGroupRenderWrapperProps,
} from '@app/features/channel/components/MessageGroup';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {type ChannelStreamItem, ChannelStreamType} from '@app/features/messaging/utils/MessageGroupingUtils';
import type {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {plural} from '@lingui/core/macro';
import {clsx} from 'clsx';
import React, {useCallback, useEffect, useMemo, useRef} from 'react';

const MESSAGE_SCROLLER_SELECTOR = '[data-fluxer-scroll-container="true"]';
const SCROLLER_BOTTOM_EPSILON = 1;

interface BlockedMessageGroupsProps {
	channel: Channel;
	messageGroups: Array<ChannelStreamItem>;
	onReveal: (messageId: string | null) => void;
	revealed: boolean;
	hasUnread?: boolean;
	compact: boolean;
	messageGroupSpacing: number;
	variant: 'blocked' | 'spammer';
	className?: string;
	messagePreviewContext?: keyof typeof MessagePreviewContext;
	messageBehaviorOverrides?: MessageGroupProps['behaviorOverrides'];
	messageRowClassName?: string;
	messageActionsClassName?: string;
	renderMessageActions?: (message: Message) => React.ReactNode;
	renderMessageWrapper?: (props: MessageGroupRenderWrapperProps) => React.ReactNode;
	suppressUnreadIndicator?: boolean;
}

const arePropsEqual = (prevProps: BlockedMessageGroupsProps, nextProps: BlockedMessageGroupsProps): boolean => {
	if (prevProps.channel.id !== nextProps.channel.id) return false;
	if (prevProps.revealed !== nextProps.revealed) return false;
	if (prevProps.hasUnread !== nextProps.hasUnread) return false;
	if (prevProps.compact !== nextProps.compact) return false;
	if (prevProps.messageGroupSpacing !== nextProps.messageGroupSpacing) return false;
	if (prevProps.variant !== nextProps.variant) return false;
	if (prevProps.className !== nextProps.className) return false;
	if (prevProps.onReveal !== nextProps.onReveal) return false;
	if (prevProps.messagePreviewContext !== nextProps.messagePreviewContext) return false;
	if (prevProps.messageBehaviorOverrides !== nextProps.messageBehaviorOverrides) return false;
	if (prevProps.messageRowClassName !== nextProps.messageRowClassName) return false;
	if (prevProps.messageActionsClassName !== nextProps.messageActionsClassName) return false;
	if (prevProps.renderMessageActions !== nextProps.renderMessageActions) return false;
	if (prevProps.renderMessageWrapper !== nextProps.renderMessageWrapper) return false;
	if (prevProps.suppressUnreadIndicator !== nextProps.suppressUnreadIndicator) return false;
	if (prevProps.messageGroups.length !== nextProps.messageGroups.length) return false;
	for (let i = 0; i < prevProps.messageGroups.length; i++) {
		const prevGroup = prevProps.messageGroups[i];
		const nextGroup = nextProps.messageGroups[i];
		if (!nextGroup) return false;
		if (prevGroup.type !== nextGroup.type) return false;
		if (prevGroup.type === ChannelStreamType.MESSAGE) {
			const prevMessage = prevGroup.content as Message;
			const nextMessage = nextGroup.content as Message;
			if (prevMessage !== nextMessage) return false;
		}
	}
	return true;
};
export const BlockedMessageGroups = React.memo<BlockedMessageGroupsProps>((props) => {
	const {
		messageGroups,
		channel,
		compact,
		revealed,
		hasUnread = false,
		messageGroupSpacing,
		onReveal,
		variant,
		className,
		messagePreviewContext,
		messageBehaviorOverrides,
		messageRowClassName,
		messageActionsClassName,
		renderMessageActions,
		renderMessageWrapper,
		suppressUnreadIndicator,
	} = props;
	const containerRef = useRef<HTMLDivElement>(null);
	const scrollToBottomFrameRef = useRef<number | null>(null);
	const messageSummary = useMemo(() => {
		let firstMessageId: string | null = null;
		let totalMessageCount = 0;
		for (const item of messageGroups) {
			if (item.type !== ChannelStreamType.MESSAGE) {
				continue;
			}
			totalMessageCount++;
			if (firstMessageId === null) {
				firstMessageId = (item.content as Message).id;
			}
		}
		return {firstMessageId, totalMessageCount};
	}, [messageGroups]);
	const scheduleScrollToBottom = useCallback((scroller: HTMLElement) => {
		if (scrollToBottomFrameRef.current != null) {
			cancelAnimationFrame(scrollToBottomFrameRef.current);
		}
		scrollToBottomFrameRef.current = requestAnimationFrame(() => {
			scrollToBottomFrameRef.current = null;
			scroller.scrollTop = scroller.scrollHeight;
		});
	}, []);
	const handleClick = useCallback(() => {
		const container = containerRef.current;
		const scroller = container?.closest(MESSAGE_SCROLLER_SELECTOR) as HTMLElement | null;
		if (scroller) {
			const wasAtBottom = scroller.scrollHeight - scroller.scrollTop - scroller.clientHeight < SCROLLER_BOTTOM_EPSILON;
			if (revealed) {
				onReveal(null);
				if (wasAtBottom) {
					scheduleScrollToBottom(scroller);
				}
			} else {
				if (messageSummary.firstMessageId) {
					onReveal(messageSummary.firstMessageId);
					if (wasAtBottom) {
						scheduleScrollToBottom(scroller);
					}
				}
			}
		} else {
			if (revealed) {
				onReveal(null);
			} else {
				if (messageSummary.firstMessageId) {
					onReveal(messageSummary.firstMessageId);
				}
			}
		}
	}, [messageSummary.firstMessageId, onReveal, revealed, scheduleScrollToBottom]);
	useEffect(() => {
		return () => {
			if (scrollToBottomFrameRef.current != null) {
				cancelAnimationFrame(scrollToBottomFrameRef.current);
			}
		};
	}, []);
	const messageNodes = useMemo(() => {
		if (!revealed) return null;
		const nodes: Array<React.ReactNode> = [];
		let currentGroupMessages: Array<Message> = [];
		let groupId: string | undefined;
		const flushGroup = () => {
			if (currentGroupMessages.length > 0) {
				nodes.push(
					<MessageGroup
						key={currentGroupMessages[0].id}
						messages={currentGroupMessages}
						channel={channel}
						messageDisplayCompact={compact}
						idPrefix={variant === 'spammer' ? 'spammer-messages' : 'blocked-messages'}
						previewContext={messagePreviewContext}
						behaviorOverrides={messageBehaviorOverrides}
						messageRowClassName={messageRowClassName}
						messageActionsClassName={messageActionsClassName}
						renderMessageActions={renderMessageActions}
						renderMessageWrapper={renderMessageWrapper}
						data-flx="channel.blocked-message-groups.flush-group.message-group"
					/>,
				);
				currentGroupMessages = [];
				groupId = undefined;
			}
		};
		messageGroups.forEach((item, itemIndex) => {
			if (item.type === ChannelStreamType.DIVIDER) {
				if (item.unreadId && suppressUnreadIndicator) {
					return;
				}
				if (itemIndex === 0 && item.unreadId) {
					return;
				}
				flushGroup();
				nodes.push(
					<Divider
						key={item.unreadId || item.contentKey || `divider-${itemIndex}`}
						spacing={messageGroupSpacing}
						red={!!item.unreadId}
						id={item.unreadId ? 'new-messages-bar' : undefined}
						data-flx="channel.blocked-message-groups.message-nodes.divider"
					>
						{item.content as string}
					</Divider>,
				);
			} else if (item.type === ChannelStreamType.MESSAGE) {
				const message = item.content as Message;
				if (groupId !== item.groupId) {
					flushGroup();
					groupId = item.groupId;
				}
				currentGroupMessages.push(message);
			}
		});
		flushGroup();
		return nodes;
	}, [
		revealed,
		messageGroups,
		messageGroupSpacing,
		channel,
		compact,
		variant,
		messagePreviewContext,
		messageBehaviorOverrides,
		messageRowClassName,
		messageActionsClassName,
		renderMessageActions,
		renderMessageWrapper,
		suppressUnreadIndicator,
	]);
	const leadingUnreadDivider = messageGroups[0]?.type === ChannelStreamType.DIVIDER && !!messageGroups[0].unreadId;
	return (
		<div
			ref={containerRef}
			className={clsx(styles.container, className)}
			data-flx="channel.blocked-message-groups.container"
		>
			{hasUnread && (!revealed || leadingUnreadDivider) && (
				<Divider
					spacing={messageGroupSpacing}
					red={true}
					id="new-messages-bar"
					data-flx="channel.blocked-message-groups.collapsed-unread-divider"
				/>
			)}
			<button
				type="button"
				className={styles.toggle}
				onClick={handleClick}
				data-flx="channel.blocked-message-groups.toggle.click.button"
			>
				{variant === 'spammer'
					? plural(
							{count: messageSummary.totalMessageCount},
							{
								one: '# potential spammer message',
								other: '# potential spammer messages',
							},
						)
					: plural(
							{count: messageSummary.totalMessageCount},
							{
								one: '# blocked message',
								other: '# blocked messages',
							},
						)}
			</button>
			{revealed && (
				<div className={styles.content} data-blocked-messages data-flx="channel.blocked-message-groups.content">
					{messageNodes}
				</div>
			)}
		</div>
	);
}, arePropsEqual);
