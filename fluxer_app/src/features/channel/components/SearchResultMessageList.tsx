// SPDX-License-Identifier: AGPL-3.0-or-later

import {BlockedMessageGroups} from '@app/features/channel/components/BlockedMessageGroups';
import type {MessageBehaviorOverrides} from '@app/features/channel/components/ChannelMessage';
import type {MessageGroupRenderWrapperProps} from '@app/features/channel/components/MessageGroup';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {
	type ChannelStreamItem,
	ChannelStreamType,
	getCollapsedGroupType,
	isNewMessageGroup,
} from '@app/features/messaging/utils/MessageGroupingUtils';
import {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import React, {useMemo} from 'react';

interface SearchResultMessageListProps {
	channel: Channel;
	messages: Array<Message>;
	revealedGroupKeys: ReadonlySet<string>;
	onGroupRevealChange: (groupKey: string, revealed: boolean) => void;
	renderMessage: (message: Message) => React.ReactNode;
	collapsedGroupClassName?: string;
	messagePreviewContext?: keyof typeof MessagePreviewContext;
	messageBehaviorOverrides?: MessageBehaviorOverrides;
	messageActionsClassName?: string;
	renderMessageActions?: (message: Message) => React.ReactNode;
	renderMessageWrapper?: (props: MessageGroupRenderWrapperProps) => React.ReactNode;
	spammerOverrideVersion?: number;
}

type SearchResultListItem =
	| {type: 'message'; message: Message}
	| {
			type: 'collapsed';
			key: string;
			variant: 'blocked' | 'spammer';
			items: Array<ChannelStreamItem>;
	  };

function buildSearchResultListItems(channel: Channel, messages: Array<Message>): Array<SearchResultListItem> {
	const items: Array<SearchResultListItem> = [];
	let pendingCollapsedType: ChannelStreamType | null = null;
	let pendingCollapsedKey: string | null = null;
	let pendingCollapsedItems: Array<ChannelStreamItem> = [];
	let pendingGroupId: string | undefined;
	let lastPendingMessage: Message | undefined;
	const flushCollapsedGroup = () => {
		if (!pendingCollapsedType || !pendingCollapsedKey || pendingCollapsedItems.length === 0) {
			return;
		}
		items.push({
			type: 'collapsed',
			key: pendingCollapsedKey,
			variant: pendingCollapsedType === ChannelStreamType.MESSAGE_GROUP_SPAMMER ? 'spammer' : 'blocked',
			items: pendingCollapsedItems,
		});
		pendingCollapsedType = null;
		pendingCollapsedKey = null;
		pendingCollapsedItems = [];
		pendingGroupId = undefined;
		lastPendingMessage = undefined;
	};
	for (const message of messages) {
		const collapsedType = getCollapsedGroupType(channel, message, true);
		if (collapsedType === null) {
			flushCollapsedGroup();
			items.push({type: 'message', message});
			continue;
		}
		if (pendingCollapsedType !== collapsedType) {
			flushCollapsedGroup();
			pendingCollapsedType = collapsedType;
			pendingCollapsedKey = message.id;
		}
		if (!pendingGroupId || isNewMessageGroup(channel, lastPendingMessage, message)) {
			pendingGroupId = message.id;
		}
		pendingCollapsedItems.push({
			type: ChannelStreamType.MESSAGE,
			content: message,
			groupId: pendingGroupId,
		});
		lastPendingMessage = message;
	}
	flushCollapsedGroup();
	return items;
}

export function SearchResultMessageList({
	channel,
	messages,
	revealedGroupKeys,
	onGroupRevealChange,
	renderMessage,
	collapsedGroupClassName,
	messagePreviewContext = MessagePreviewContext.LIST_POPOUT,
	messageBehaviorOverrides,
	messageActionsClassName,
	renderMessageActions,
	renderMessageWrapper,
	spammerOverrideVersion,
}: SearchResultMessageListProps): React.ReactNode {
	const items = useMemo(
		() => buildSearchResultListItems(channel, messages),
		[channel, messages, spammerOverrideVersion],
	);
	return items.map((item) => {
		if (item.type === 'message') {
			return <React.Fragment key={item.message.id}>{renderMessage(item.message)}</React.Fragment>;
		}
		return (
			<BlockedMessageGroups
				key={`${item.variant}-${item.key}`}
				channel={channel}
				messageGroups={item.items}
				revealed={revealedGroupKeys.has(item.key)}
				compact={false}
				messageGroupSpacing={8}
				onReveal={(messageId) => {
					onGroupRevealChange(item.key, messageId != null);
				}}
				variant={item.variant}
				className={collapsedGroupClassName}
				messagePreviewContext={messagePreviewContext}
				messageBehaviorOverrides={messageBehaviorOverrides}
				messageActionsClassName={messageActionsClassName}
				renderMessageActions={renderMessageActions}
				renderMessageWrapper={renderMessageWrapper}
				data-flx="channel.search-result-message-list.blocked-message-groups"
			/>
		);
	});
}
