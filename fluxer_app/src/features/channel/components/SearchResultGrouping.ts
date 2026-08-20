// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';

export interface SearchResultGroup {
	key: string;
	channelId: string;
	messages: Array<Message>;
}

export const buildSearchResultGroups = (messages: Array<Message>): Array<SearchResultGroup> => {
	const groups: Array<SearchResultGroup> = [];
	let currentGroup: SearchResultGroup | null = null;
	for (const message of messages) {
		if (!currentGroup || currentGroup.channelId !== message.channelId) {
			currentGroup = {key: `${message.channelId}-${message.id}`, channelId: message.channelId, messages: []};
			groups.push(currentGroup);
		}
		currentGroup.messages.push(message);
	}
	return groups;
};

export const buildSearchResultGroupsByMessageId = (
	groups: Array<SearchResultGroup>,
): Map<string, SearchResultGroup> => {
	const groupsByMessageId = new Map<string, SearchResultGroup>();
	for (const group of groups) {
		for (const message of group.messages) {
			groupsByMessageId.set(message.id, group);
		}
	}
	return groupsByMessageId;
};

export const countSearchResultChannels = (groups: Array<SearchResultGroup>): number => {
	return new Set(groups.map((group) => group.channelId)).size;
};
