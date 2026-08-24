// SPDX-License-Identifier: AGPL-3.0-or-later

import {Message} from '@app/features/messaging/models/MessagingMessage';
import type {SavedMessageEntry, SavedMessageMissingEntry} from '@app/features/messaging/models/SavedMessageEntry';
import type {Channel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {makeAutoObservable} from 'mobx';

class SavedMessages {
	savedMessages: Array<Message> = [];
	missingSavedMessages: Array<SavedMessageMissingEntry> = [];
	fetched = false;

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

	fetchSuccess(entries: ReadonlyArray<SavedMessageEntry>): void {
		this.savedMessages = entries
			.filter((entry) => entry.status === 'available' && entry.message)
			.map((entry) => entry.message!)
			.sort((a, b) => (b.id > a.id ? 1 : a.id > b.id ? -1 : 0));
		this.missingSavedMessages = entries
			.filter((entry) => entry.status === 'missing_permissions' || entry.message === null)
			.map((entry) => entry.toMissingEntry());
		this.fetched = true;
	}

	fetchError(): void {
		this.savedMessages = [];
		this.missingSavedMessages = [];
		this.fetched = false;
	}

	handleGatewayReady(): void {
		this.savedMessages = [];
		this.missingSavedMessages = [];
		this.fetched = false;
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
