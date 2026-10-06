// SPDX-License-Identifier: AGPL-3.0-or-later

export const CHANNEL_MESSAGE_ID_PREFIX = 'chat-messages';

export const escapeSelectorValue = (value: string): string =>
	value.replace(/[\\"]/gu, '\\$&').replace(/[\n\r\f]/gu, (char) => `\\${char.charCodeAt(0).toString(16)} `);

export const getMessageSelector = (channelId?: string, messageId?: string): string => {
	const channelSelector = channelId ? `[data-channel-id="${escapeSelectorValue(channelId)}"]` : '[data-channel-id]';
	const messageSelector = messageId ? `[data-message-id="${escapeSelectorValue(messageId)}"]` : '[data-message-id]';
	return `${channelSelector}${messageSelector}`;
};

export const findMessageElement = (
	doc: Document | null | undefined,
	viewport: HTMLElement | null | undefined,
	channelId: string,
	messageId: string,
): HTMLElement | null => {
	const byId = doc?.getElementById(`${CHANNEL_MESSAGE_ID_PREFIX}-${channelId}-${messageId}`) ?? null;
	if (byId != null) {
		return byId as HTMLElement;
	}
	return viewport?.querySelector<HTMLElement>(getMessageSelector(channelId, messageId)) ?? null;
};
