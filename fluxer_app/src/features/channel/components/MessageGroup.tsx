// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type MessageBehaviorOverrides,
	Message as MessageComponent,
} from '@app/features/channel/components/ChannelMessage';
import {UnreadDividerSlot} from '@app/features/channel/components/UnreadDividerSlot';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Fragment, memo, useMemo} from 'react';

const MESSAGE_GROUP_DESCRIPTOR = msg({
	message: 'Message group',
	comment: 'Short label in the channel and chat message group. Keep it concise.',
});

export interface MessageGroupRenderWrapperProps {
	message: Message;
	index: number;
	isGroupStart: boolean;
	children: React.ReactNode;
	className?: string;
}

export interface MessageGroupProps {
	messages: Array<Message>;
	channel: Channel;
	onEdit?: (targetNode: HTMLElement) => void;
	jumpTicket?: number;
	highlightedMessageId?: string | null;
	messageDisplayCompact?: boolean;
	flashKey?: number;
	showUnreadDividerSlots?: boolean;
	unreadDividerBeforeMessageId?: string | null;
	idPrefix?: string;
	messageRowClassName?: string;
	messageActionsClassName?: string;
	renderMessageActions?: (message: Message) => React.ReactNode;
	suppressMessageActions?: boolean;
	previewContext?: keyof typeof MessagePreviewContext;
	behaviorOverrides?: MessageBehaviorOverrides;
	renderMessageWrapper?: (props: MessageGroupRenderWrapperProps) => React.ReactNode;
	getMessageHeadingActivate?: (message: Message) => (() => void) | undefined;
}

const MessageGroupBase: React.FC<MessageGroupProps> = observer((props) => {
	const {i18n} = useLingui();
	const {
		messages,
		channel,
		onEdit,
		jumpTicket,
		highlightedMessageId,
		messageDisplayCompact = false,
		showUnreadDividerSlots = false,
		unreadDividerBeforeMessageId = null,
		idPrefix,
		messageRowClassName,
		messageActionsClassName,
		renderMessageActions,
		suppressMessageActions,
		previewContext,
		behaviorOverrides: providedBehaviorOverrides,
		renderMessageWrapper,
		getMessageHeadingActivate,
	} = props;
	const groupId = useMemo(() => messages[0]?.id, [messages]);
	const behaviorOverrides = useMemo(
		() =>
			suppressMessageActions
				? {
						...providedBehaviorOverrides,
						disableContextMenu: true,
					}
				: providedBehaviorOverrides,
		[suppressMessageActions, providedBehaviorOverrides],
	);
	const renderedMessages = useMemo(
		() =>
			messages.map((message, index) => {
				const prevMessage = messages[index - 1];
				const isGroupStart = index === 0;
				const messageContent = (
					<>
						<MessageComponent
							channel={channel}
							message={message}
							prevMessage={prevMessage}
							onEdit={onEdit}
							shouldGroup={!isGroupStart}
							isJumpTarget={highlightedMessageId === message.id}
							compact={messageDisplayCompact}
							idPrefix={idPrefix}
							behaviorOverrides={behaviorOverrides}
							suppressMessageActions={suppressMessageActions}
							previewContext={previewContext}
							onHeadingActivate={getMessageHeadingActivate?.(message)}
							data-flx="channel.message-group.rendered-messages.message-component"
						/>
						{renderMessageActions && (
							<div className={messageActionsClassName} data-flx="channel.message-group.rendered-messages.div">
								{renderMessageActions(message)}
							</div>
						)}
					</>
				);
				return (
					<Fragment key={message.id}>
						{showUnreadDividerSlots && (
							<UnreadDividerSlot
								beforeId={message.id}
								visible={unreadDividerBeforeMessageId === message.id}
								data-flx="channel.message-group.rendered-messages.unread-divider-slot"
							/>
						)}
						{renderMessageWrapper ? (
							renderMessageWrapper({
								message,
								index,
								isGroupStart,
								children: messageContent,
								className: messageRowClassName,
							})
						) : (
							<div
								data-message-index={index}
								data-message-id={message.id}
								data-is-group-start={isGroupStart}
								className={messageRowClassName}
								data-flx="channel.message-group.rendered-messages.div--2"
							>
								{messageContent}
							</div>
						)}
					</Fragment>
				);
			}),
		[
			messages,
			channel,
			onEdit,
			highlightedMessageId,
			messageDisplayCompact,
			idPrefix,
			showUnreadDividerSlots,
			unreadDividerBeforeMessageId,
			messageRowClassName,
			messageActionsClassName,
			renderMessageActions,
			behaviorOverrides,
			suppressMessageActions,
			previewContext,
			renderMessageWrapper,
			getMessageHeadingActivate,
		],
	);
	return (
		<div
			data-jump-sequence-id={jumpTicket}
			data-group-id={groupId}
			role="group"
			aria-label={i18n._(MESSAGE_GROUP_DESCRIPTOR)}
			data-flx="channel.message-group.group"
		>
			{renderedMessages}
		</div>
	);
});

function areMessagesEqual(previous: Array<Message>, next: Array<Message>): boolean {
	if (previous === next) {
		return true;
	}
	if (previous.length !== next.length) {
		return false;
	}
	for (let index = 0; index < previous.length; index++) {
		if (previous[index] !== next[index]) {
			return false;
		}
	}
	return true;
}

function arePropsEqual(previous: MessageGroupProps, next: MessageGroupProps): boolean {
	const previousKeys = Object.keys(previous) as Array<keyof MessageGroupProps>;
	if (previousKeys.length !== Object.keys(next).length) {
		return false;
	}
	for (const key of previousKeys) {
		if (key === 'messages') {
			continue;
		}
		if (previous[key] !== next[key]) {
			return false;
		}
	}
	return areMessagesEqual(previous.messages, next.messages);
}

export const MessageGroup = memo(MessageGroupBase, arePropsEqual);
