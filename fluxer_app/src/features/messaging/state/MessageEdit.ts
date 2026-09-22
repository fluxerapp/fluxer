// SPDX-License-Identifier: AGPL-3.0-or-later

import TextareaSelection from '@app/features/messaging/state/TextareaSelection';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {compareStructural, makeAutoObservable, reaction} from 'mobx';

const MESSAGE_EDIT_STORAGE_KEY = 'MessageEdit';

class MessageEdit {
	private editingMessageIds: Record<string, string> = {};
	private editingContents: Record<string, string> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		AppStorage.removeItem(MESSAGE_EDIT_STORAGE_KEY);
	}

	startEditing(channelId: string, messageId: string, initialContent: string): void {
		const currentMessageId = this.editingMessageIds[channelId];
		const currentContent = this.editingContents[messageId];
		if (currentMessageId === messageId && currentContent === initialContent) {
			return;
		}
		if (currentMessageId !== messageId) {
			if (currentMessageId) {
				TextareaSelection.clearEditingSelection(channelId, currentMessageId);
			}
			this.editingMessageIds[channelId] = messageId;
		}
		if (currentContent !== initialContent) {
			this.editingContents[messageId] = initialContent;
		}
	}

	stopEditing(channelId: string): void {
		if (!(channelId in this.editingMessageIds)) {
			return;
		}
		const messageId = this.editingMessageIds[channelId];
		if (messageId) {
			TextareaSelection.clearEditingSelection(channelId, messageId);
		}
		delete this.editingMessageIds[channelId];
	}

	isEditing(channelId: string, messageId: string): boolean {
		return this.editingMessageIds[channelId] === messageId;
	}

	getEditingMessageId(channelId: string): string | null {
		return this.editingMessageIds[channelId] ?? null;
	}

	setEditingContent(channelId: string, messageId: string, content: string): void {
		if (this.editingMessageIds[channelId] !== messageId) {
			return;
		}
		if (this.editingContents[messageId] === content) {
			return;
		}
		this.editingContents[messageId] = content;
	}

	getEditingContent(channelId: string, messageId: string): string | null {
		if (this.editingMessageIds[channelId] !== messageId) {
			return null;
		}
		return this.editingContents[messageId] ?? null;
	}

	getDraftContent(messageId: string): string | null {
		return this.editingContents[messageId] ?? null;
	}

	clearDraftContent(messageId: string): void {
		if (!(messageId in this.editingContents)) {
			return;
		}
		delete this.editingContents[messageId];
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => Object.entries(this.editingMessageIds),
			() => callback(),
			{fireImmediately: true, equals: compareStructural},
		);
	}
}

export default new MessageEdit();
