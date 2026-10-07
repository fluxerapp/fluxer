// SPDX-License-Identifier: AGPL-3.0-or-later

import TextareaSelection from '@app/features/messaging/state/TextareaSelection';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import {makeAutoObservable} from 'mobx';

class MessageEditMobile {
	editingMessageIds: Record<string, string> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	startEditingMobile(channelId: string, messageId: string): void {
		if (this.editingMessageIds[channelId] === messageId) {
			return;
		}
		const currentMessageId = this.editingMessageIds[channelId];
		if (currentMessageId) {
			TextareaSelection.clearEditingSelection(channelId, currentMessageId);
		}
		this.editingMessageIds[channelId] = messageId;
	}

	stopEditingMobile(channelId: string): void {
		if (!(channelId in this.editingMessageIds)) {
			return;
		}
		const messageId = this.editingMessageIds[channelId];
		if (messageId) {
			TextareaSelection.clearEditingSelection(channelId, messageId);
		}
		delete this.editingMessageIds[channelId];
	}

	isEditingMobile(channelId: string, messageId: string): boolean {
		return this.editingMessageIds[channelId] === messageId;
	}

	getEditingMobileMessageId(channelId: string): string | null {
		return this.editingMessageIds[channelId] ?? null;
	}

	reset(): void {
		this.editingMessageIds = {};
	}
}

const messageEditMobile = new MessageEditMobile();

AccountScopedWork.registerCancellation(() => messageEditMobile.reset());

export default messageEditMobile;
