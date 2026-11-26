// SPDX-License-Identifier: AGPL-3.0-or-later

import {compareStructural, makeAutoObservable, reaction} from 'mobx';

class PersonaChange {
	private editingMessageIds: Record<string, string> = {};
	//private editingContents: Record<string, string> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	startEditing(channelId: string, messageId: string): void {
		const currentMessageId = this.editingMessageIds[channelId];
		if (currentMessageId === messageId) {
			return;
		}
		this.editingMessageIds[channelId] = messageId;
	}

	stopEditing(channelId: string): void {
		if (!(channelId in this.editingMessageIds)) {
			return;
		}
		delete this.editingMessageIds[channelId];
	}

	isEditing(channelId: string, messageId: string): boolean {
		return this.editingMessageIds[channelId] === messageId;
	}

	getEditingMessageId(channelId: string): string | null {
		return this.editingMessageIds[channelId] ?? null;
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => Object.entries(this.editingMessageIds),
			() => callback(),
			{fireImmediately: true, equals: compareStructural},
		);
	}
}

export default new PersonaChange();
