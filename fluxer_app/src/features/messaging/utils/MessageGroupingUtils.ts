// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {ChannelMessages} from '@app/features/messaging/state/ChannelMessages';
import LocalUserSpamOverride from '@app/features/moderation/state/LocalUserSpamOverride';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {MessageFlags, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import {isSameDay as isSameDayBase} from '@fluxer/date_utils/src/DateComparison';
import {extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';

export const ChannelStreamType = {
	MESSAGE: 'MESSAGE',
	MESSAGE_GROUP_BLOCKED: 'MESSAGE_GROUP_BLOCKED',
	MESSAGE_GROUP_IGNORED: 'MESSAGE_GROUP_IGNORED',
	MESSAGE_GROUP_SPAMMER: 'MESSAGE_GROUP_SPAMMER',
	DIVIDER: 'DIVIDER',
} as const;

export type ChannelStreamType = ValueOf<typeof ChannelStreamType>;

export interface ChannelStreamItem {
	type: ChannelStreamType;
	content: Message | Array<ChannelStreamItem> | string;
	groupId?: string;
	key?: string;
	flashKey?: number;
	jumpTarget?: boolean;
	hasUnread?: boolean;
	hasJumpTarget?: boolean;
	unreadId?: string;
	contentKey?: string;
	showUnreadDividerBefore?: boolean;
}

export const MESSAGE_GROUP_TIMEOUT = 7 * 60 * 1000;

export function isNewMessageGroup(
	_channel: Channel | undefined,
	prevMessage: Message | undefined,
	currentMessage: Message,
): boolean {
	if (!prevMessage) {
		return true;
	}
	if (currentMessage.type === MessageTypes.REPLY) {
		return true;
	}
	const currentIsDisplaySystem =
		currentMessage.type !== MessageTypes.DEFAULT && currentMessage.type !== MessageTypes.REPLY;
	const prevIsDisplaySystem = prevMessage.type !== MessageTypes.DEFAULT && prevMessage.type !== MessageTypes.REPLY;
	if (currentIsDisplaySystem !== prevIsDisplaySystem) {
		return true;
	}
	const isCurrentUserContent = currentMessage.isUserMessage();
	const isPrevUserContent = prevMessage.isUserMessage();
	const bothDisplaySystem = currentIsDisplaySystem && prevIsDisplaySystem;
	if (!bothDisplaySystem && currentMessage.type !== prevMessage.type && !(isCurrentUserContent && isPrevUserContent)) {
		return true;
	}
	if (prevMessage.type <= MessageTypes.REPLY && prevMessage.author.id !== currentMessage.author.id) {
		return true;
	}
	if (currentMessage.webhookId && prevMessage.author.username !== currentMessage.author.username) {
		return true;
	}
	if (!prevMessage.timestamp || !currentMessage.timestamp) {
		return true;
	}
	if (!isSameDayBase(prevMessage.timestamp, currentMessage.timestamp)) {
		return true;
	}
	const timeDiff = currentMessage.timestamp.getTime() - prevMessage.timestamp.getTime();
	if (timeDiff > MESSAGE_GROUP_TIMEOUT) {
		return true;
	}
	const prevSuppressed = prevMessage.hasFlag(MessageFlags.SUPPRESS_NOTIFICATIONS);
	const currSuppressed = currentMessage.hasFlag(MessageFlags.SUPPRESS_NOTIFICATIONS);
	if (currSuppressed !== prevSuppressed) {
		if (!prevSuppressed && currSuppressed) {
			return true;
		}
		if (prevSuppressed && !currSuppressed) {
			const hasMentions =
				currentMessage.mentions.length > 0 || currentMessage.mentionRoles.length > 0 || currentMessage.mentionEveryone;
			if (hasMentions) {
				return true;
			}
		}
	}
	return false;
}

export function getCollapsedGroupType(
	_channel: Channel,
	message: Message,
	treatSpam: boolean,
): ChannelStreamType | null {
	if (message.blocked) {
		return ChannelStreamType.MESSAGE_GROUP_BLOCKED;
	}
	if (
		treatSpam &&
		message.author.id !== Authentication.currentUserId &&
		LocalUserSpamOverride.isUserMarkedAsSpammer(message.author.id, message.author.flags)
	) {
		return ChannelStreamType.MESSAGE_GROUP_SPAMMER;
	}
	return null;
}

interface CollapsedMessageGroupKeyOptions {
	channel: Channel;
	messages: {
		forEach(callback: (message: Message, index: number) => boolean | undefined): void;
	};
	messageId: string;
	treatSpam: boolean;
}

export function getCollapsedMessageGroupKey({
	channel,
	messages,
	messageId,
	treatSpam,
}: CollapsedMessageGroupKeyOptions): string | null {
	let currentType: ChannelStreamType | null = null;
	let currentGroupKey: string | null = null;
	let matchingGroupKey: string | null = null;
	messages.forEach((message) => {
		if (matchingGroupKey !== null) {
			return false;
		}
		const collapsedType = getCollapsedGroupType(channel, message, treatSpam);
		if (collapsedType === null) {
			currentType = null;
			currentGroupKey = null;
			return undefined;
		}
		if (collapsedType !== currentType) {
			currentType = collapsedType;
			currentGroupKey = message.id;
		}
		if (message.id === messageId) {
			matchingGroupKey = currentGroupKey;
			return false;
		}
		return undefined;
	});
	return matchingGroupKey;
}

export function createChannelStream(props: {
	channel: Channel;
	messages: ChannelMessages;
	oldestUnreadMessageId: string | null;
	treatSpam: boolean;
}): Array<ChannelStreamItem> {
	const {channel, messages, oldestUnreadMessageId, treatSpam} = props;
	const stream: Array<ChannelStreamItem> = [];
	let lastDateDividerTimestamp: Date | undefined;
	let groupId: string | undefined;
	let lastMessageInGroup: Message | undefined;
	let unreadTimestamp: number | null = oldestUnreadMessageId ? extractTimestamp(oldestUnreadMessageId) : null;
	messages.forEach((message): boolean | undefined => {
		if (!lastDateDividerTimestamp || !isSameDayBase(lastDateDividerTimestamp, message.timestamp)) {
			const dateString = DateUtils.getFormattedFullDate(message.timestamp);
			stream.push({
				type: ChannelStreamType.DIVIDER,
				content: dateString,
				contentKey: dateString,
			});
			lastDateDividerTimestamp = message.timestamp;
		}
		const lastItem = stream[stream.length - 1];
		const previousWasCollapsedGroup =
			lastItem?.type === ChannelStreamType.MESSAGE_GROUP_BLOCKED ||
			lastItem?.type === ChannelStreamType.MESSAGE_GROUP_IGNORED ||
			lastItem?.type === ChannelStreamType.MESSAGE_GROUP_SPAMMER;
		let collapsedGroupItem: ChannelStreamItem | null = null;
		let lastInCollapsedGroup: ChannelStreamItem | undefined;
		const collapsedType = getCollapsedGroupType(channel, message, treatSpam);
		if (collapsedType !== null) {
			if (lastItem?.type !== collapsedType) {
				collapsedGroupItem = {
					type: collapsedType,
					content: [],
					key: message.id,
				};
				stream.push(collapsedGroupItem);
			} else {
				collapsedGroupItem = lastItem;
				const collapsedContent = collapsedGroupItem.content as Array<ChannelStreamItem>;
				lastInCollapsedGroup = collapsedContent[collapsedContent.length - 1];
			}
		}
		let shouldShowUnreadDividerBefore = false;
		let shouldOpenCollapsedGroupWithUnreadDivider = false;
		if (oldestUnreadMessageId === message.id && unreadTimestamp != null) {
			if (lastItem?.type === ChannelStreamType.DIVIDER) {
				lastItem.unreadId = message.id;
			} else if (collapsedGroupItem !== null) {
				shouldOpenCollapsedGroupWithUnreadDivider = true;
			} else {
				shouldShowUnreadDividerBefore = true;
			}
			unreadTimestamp = null;
		} else if (unreadTimestamp != null && extractTimestamp(message.id) > unreadTimestamp) {
			if (collapsedGroupItem !== null) {
				shouldOpenCollapsedGroupWithUnreadDivider = true;
			} else {
				shouldShowUnreadDividerBefore = true;
			}
			unreadTimestamp = null;
		}
		let prevMessageForGrouping: Message | undefined;
		if (collapsedGroupItem && lastInCollapsedGroup && lastInCollapsedGroup.type === ChannelStreamType.MESSAGE) {
			prevMessageForGrouping = lastInCollapsedGroup.content as Message;
		} else if (previousWasCollapsedGroup && collapsedGroupItem == null) {
			prevMessageForGrouping = undefined;
		} else if (lastItem?.type === ChannelStreamType.MESSAGE) {
			prevMessageForGrouping = lastMessageInGroup ?? (lastItem.content as Message);
		} else {
			prevMessageForGrouping = lastMessageInGroup;
		}
		const shouldStartNewGroup = isNewMessageGroup(channel, prevMessageForGrouping, message);
		if (shouldStartNewGroup) {
			groupId = message.id;
		}
		const messageItem: ChannelStreamItem = {
			type: ChannelStreamType.MESSAGE,
			content: message,
			groupId,
			showUnreadDividerBefore: shouldShowUnreadDividerBefore,
		};
		if (groupId === message.id) {
			lastMessageInGroup = message;
		}
		const {jumpTicket, jumpHighlight, jumpDestinationId} = messages;
		if (jumpHighlight && message.id === jumpDestinationId && jumpTicket != null) {
			messageItem.flashKey = jumpTicket;
		}
		if (messages.jumpDestinationId === message.id) {
			messageItem.jumpTarget = true;
		}
		if (collapsedGroupItem !== null) {
			const collapsedContentItems = collapsedGroupItem.content as Array<ChannelStreamItem>;
			if (shouldOpenCollapsedGroupWithUnreadDivider) {
				collapsedContentItems.push({type: ChannelStreamType.DIVIDER, content: '', unreadId: message.id});
				collapsedGroupItem.hasUnread = true;
			}
			collapsedContentItems.push(messageItem);
			if (messageItem.jumpTarget) {
				collapsedGroupItem.hasJumpTarget = true;
			}
		} else {
			stream.push(messageItem);
		}
		return undefined;
	});
	return stream;
}
