/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable} from 'mobx';
import type {Channel} from '~/records/ChannelRecord';
import {type Message, MessageRecord} from '~/records/MessageRecord';
import type {SavedMessageEntryRecord, SavedMessageMissingEntry} from '~/records/SavedMessageEntryRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import type {ReactionEmoji} from '~/utils/ReactionUtils';

class SavedMessagesStore {
	savedMessages: Array<MessageRecord> = [];
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

	fetchSuccess(entries: ReadonlyArray<SavedMessageEntryRecord>): void {
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

	handleChannelDelete(channel: Channel): void {
		this.savedMessages = this.savedMessages.filter((message) => message.channelId !== channel.id);
		this.missingSavedMessages = this.missingSavedMessages.filter((entry) => entry.channelId !== channel.id);
	}

	handleMessageUpdate(message: Message): void {
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

	handleMessageCreate(message: Message): void {
		this.missingSavedMessages = this.missingSavedMessages.filter((entry) => entry.id !== message.id);
		this.savedMessages = [new MessageRecord(message), ...this.savedMessages];
	}

	private updateMessageWithReaction(messageId: string, updater: (message: MessageRecord) => MessageRecord): void {
		const index = this.savedMessages.findIndex((m) => m.id === messageId);
		if (index === -1) return;

		this.savedMessages = [
			...this.savedMessages.slice(0, index),
			updater(this.savedMessages[index]),
			...this.savedMessages.slice(index + 1),
		];
	}

	handleMessageReactionAdd(messageId: string, userId: string, emoji: ReactionEmoji): void {
		this.updateMessageWithReaction(messageId, (message) =>
			message.withReaction(emoji, true, userId === AuthenticationStore.currentUserId),
		);
	}

	handleMessageReactionRemove(messageId: string, userId: string, emoji: ReactionEmoji): void {
		this.updateMessageWithReaction(messageId, (message) =>
			message.withReaction(emoji, false, userId === AuthenticationStore.currentUserId),
		);
	}

	handleMessageReactionRemoveAll(messageId: string): void {
		this.updateMessageWithReaction(messageId, (message) => message.withUpdates({reactions: []}));
	}

	handleMessageReactionRemoveEmoji(messageId: string, emoji: ReactionEmoji): void {
		this.updateMessageWithReaction(messageId, (message) => message.withoutReactionEmoji(emoji));
	}
}

export default new SavedMessagesStore();
