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
import type {ScheduledMessageRecord} from '~/records/ScheduledMessageRecord';

class ScheduledMessagesStore {
	scheduledMessages: Array<ScheduledMessageRecord> = [];
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

	fetchSuccess(messages: Array<ScheduledMessageRecord>): void {
		this.scheduledMessages = sortScheduledMessages(messages);
		this.fetching = false;
		this.fetched = true;
	}

	fetchError(): void {
		this.fetching = false;
		this.fetched = false;
		this.scheduledMessages = [];
	}

	upsert(message: ScheduledMessageRecord): void {
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

function sortScheduledMessages(messages: Array<ScheduledMessageRecord>): Array<ScheduledMessageRecord> {
	return [...messages].sort((a, b) => a.scheduledAt.getTime() - b.scheduledAt.getTime());
}

export default new ScheduledMessagesStore();
