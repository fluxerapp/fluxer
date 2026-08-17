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
import {Fragment, useMemo} from 'react';

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
	jumpSequenceId?: number;
	highlightedMessageId?: string | null;
	messageDisplayCompact?: boolean;
	flashKey?: number;
	getUnreadDividerVisibility?: (messageId: string, position: 'before' | 'after') => boolean;
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

export const MessageGroup: React.FC<MessageGroupProps> = observer((props) => {
	const {i18n} = useLingui();
	const {
		messages,
		channel,
		onEdit,
		jumpSequenceId,
		highlightedMessageId,
		messageDisplayCompact = false,
		getUnreadDividerVisibility,
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
						{getUnreadDividerVisibility && (
							<UnreadDividerSlot
								beforeId={message.id}
								visible={getUnreadDividerVisibility(message.id, 'before')}
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
			getUnreadDividerVisibility,
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
			data-jump-sequence-id={jumpSequenceId}
			data-group-id={groupId}
			role="group"
			aria-label={i18n._(MESSAGE_GROUP_DESCRIPTOR)}
			data-flx="channel.message-group.group"
		>
			{renderedMessages}
		</div>
	);
});
