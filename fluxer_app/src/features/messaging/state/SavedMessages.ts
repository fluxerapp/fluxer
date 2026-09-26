// SPDX-License-Identifier: AGPL-3.0-or-later

import {Message} from '@app/features/messaging/models/MessagingMessage';
import type {SavedMessageEntry, SavedMessageMissingEntry} from '@app/features/messaging/models/SavedMessageEntry';
import {SAVED_MESSAGES_PAGE_SIZE} from '@fluxer/constants/src/LimitConstants';
import type {Channel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {makeAutoObservable} from 'mobx';

function byIdDescending(a: {id: string}, b: {id: string}): number {
	return b.id > a.id ? 1 : a.id > b.id ? -1 : 0;
}

class SavedMessages {
	savedMessages: Array<Message> = [];
	missingSavedMessages: Array<SavedMessageMissingEntry> = [];
	fetched = false;
	hasMore = true;
	isLoadingMore = false;
	cursor: string | null = null;
	private fetchGeneration = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	isSaved(messageId: string): boolean {
		return (
			this.savedMessages.some((message) => message.id === messageId) ||
			this.missingSavedMessages.some((entry) => entry.id === messageId)
		);
	}

	getMissingEntries(): Array<SavedMessageMissingEntry> {
		return this.missingSavedMessages.slice();
	}

	getHasMore(): boolean {
		return this.hasMore;
	}

	getIsLoadingMore(): boolean {
		return this.isLoadingMore;
	}

	getCursor(): string | null {
		return this.cursor;
	}

	handleFetchPending(): number {
		this.isLoadingMore = true;
		this.fetchGeneration++;
		return this.fetchGeneration;
	}

	fetchSuccess(requestId: number, entries: ReadonlyArray<SavedMessageEntry>, append: boolean): void {
		if (requestId !== this.fetchGeneration) return;
		const available = entries
			.filter((entry) => entry.status === 'available' && entry.message)
			.map((entry) => entry.message!);
		const missing = entries
			.filter((entry) => entry.status === 'missing_permissions' || entry.message === null)
			.map((entry) => entry.toMissingEntry());
		if (append) {
			const knownMessageIds = new Set(this.savedMessages.map((message) => message.id));
			const knownMissingIds = new Set(this.missingSavedMessages.map((entry) => entry.id));
			this.savedMessages = [
				...this.savedMessages,
				...available.filter((message) => !knownMessageIds.has(message.id)),
			].sort(byIdDescending);
			this.missingSavedMessages = [
				...this.missingSavedMessages,
				...missing.filter((entry) => !knownMissingIds.has(entry.id)),
			];
		} else {
			this.savedMessages = available.sort(byIdDescending);
			this.missingSavedMessages = missing;
		}
		if (entries.length > 0) {
			this.cursor = entries[entries.length - 1].messageId;
		}
		this.hasMore = entries.length === SAVED_MESSAGES_PAGE_SIZE;
		this.isLoadingMore = false;
		this.fetched = true;
	}

	fetchError(requestId: number, append: boolean): void {
		if (requestId !== this.fetchGeneration) return;
		this.isLoadingMore = false;
		if (append) return;
		this.reset();
	}

	private reset(): void {
		this.savedMessages = [];
		this.missingSavedMessages = [];
		this.fetched = false;
		this.hasMore = true;
		this.isLoadingMore = false;
		this.cursor = null;
	}

	handleGatewayReady(): void {
		this.reset();
		this.fetchGeneration++;
	}

	handleChannelDelete(channel: Channel): void {
		this.savedMessages = this.savedMessages.filter((message) => message.channelId !== channel.id);
		this.missingSavedMessages = this.missingSavedMessages.filter((entry) => entry.channelId !== channel.id);
	}

	handleMessageUpdate(message: WireMessage): void {
		const index = this.savedMessages.findIndex((m) => m.id === message.id);
		if (index === -1) return;
		this.savedMessages = [
			...this.savedMessages.slice(0, index),
			this.savedMessages[index].withUpdates(message),
			...this.savedMessages.slice(index + 1),
		];
	}

	handleMessageDelete(messageId: string): void {
		this.savedMessages = this.savedMessages.filter((message) => message.id !== messageId);
		this.missingSavedMessages = this.missingSavedMessages.filter((entry) => entry.id !== messageId);
	}

	handleMessageCreate(message: WireMessage): void {
		this.missingSavedMessages = this.missingSavedMessages.filter((entry) => entry.id !== message.id);
		this.savedMessages = [new Message(message, {missingReactions: 'preserve'}), ...this.savedMessages];
	}

	private touchMessage(messageId: string): void {
		const index = this.savedMessages.findIndex((m) => m.id === messageId);
		if (index === -1) return;
		this.savedMessages = [
			...this.savedMessages.slice(0, index),
			this.savedMessages[index].withUpdates({}),
			...this.savedMessages.slice(index + 1),
		];
	}

	handleMessageReactionAdd(messageId: string): void {
		this.touchMessage(messageId);
	}

	handleMessageReactionRemove(messageId: string): void {
		this.touchMessage(messageId);
	}

	handleMessageReactionRemoveAll(messageId: string): void {
		this.touchMessage(messageId);
	}

	handleMessageReactionRemoveEmoji(messageId: string): void {
		this.touchMessage(messageId);
	}
}

export default new SavedMessages();
