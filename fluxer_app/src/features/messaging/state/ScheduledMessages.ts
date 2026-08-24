// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScheduledMessage} from '@app/features/messaging/models/ScheduledMessage';
import {makeAutoObservable} from 'mobx';

class ScheduledMessages {
	scheduledMessages: Array<ScheduledMessage> = [];
	fetched = false;
	fetching = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get hasScheduledMessages(): boolean {
		return this.scheduledMessages.length > 0;
	}

	fetchStart(): void {
		this.fetching = true;
	}

	fetchSuccess(messages: Array<ScheduledMessage>): void {
		this.scheduledMessages = sortScheduledMessages(messages);
		this.fetching = false;
		this.fetched = true;
	}

	fetchError(): void {
		this.fetching = false;
		this.fetched = false;
		this.scheduledMessages = [];
	}

	handleGatewayReady(): void {
		this.scheduledMessages = [];
		this.fetched = false;
		this.fetching = false;
	}

	upsert(message: ScheduledMessage): void {
		const existingIndex = this.scheduledMessages.findIndex((entry) => entry.id === message.id);
		const next = [...this.scheduledMessages];
		if (existingIndex === -1) {
			next.push(message);
		} else {
			next[existingIndex] = message;
		}
		this.scheduledMessages = sortScheduledMessages(next);
	}

	remove(messageId: string): void {
		this.scheduledMessages = this.scheduledMessages.filter((message) => message.id !== messageId);
	}
}

function sortScheduledMessages(messages: Array<ScheduledMessage>): Array<ScheduledMessage> {
	return [...messages].sort((a, b) => a.scheduledAt.getTime() - b.scheduledAt.getTime());
}

export default new ScheduledMessages();
