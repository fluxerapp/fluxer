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

import {action, makeAutoObservable} from 'mobx';
import type {Message} from '~/records/MessageRecord';

type TypingUser = Readonly<{
	timeout: NodeJS.Timeout;
	expiresAt: number;
}>;

const TYPING_TIMEOUT = 10_000;

class TypingStore {
	typingUsersByChannel: Record<string, Record<string, TypingUser>> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getTypingUsers(channelId: string): ReadonlyArray<string> {
		return Object.keys(this.typingUsersByChannel[channelId] ?? {});
	}

	isTyping(channelId: string, userId: string): boolean {
		return this.typingUsersByChannel[channelId]?.[userId] !== undefined;
	}

	getCount(channelId: string): number {
		return Object.keys(this.typingUsersByChannel[channelId] ?? {}).length;
	}

	@action
	reset(): void {
		this.clearAllTimeouts();
		this.typingUsersByChannel = {};
	}

	@action
	startTyping(channelId: string, userId: string): void {
		const existingTimeout = this.typingUsersByChannel[channelId]?.[userId]?.timeout;
		if (existingTimeout) {
			clearTimeout(existingTimeout);
		}

		const newTimeout = this.scheduleClear(channelId, userId);

		if (!this.typingUsersByChannel[channelId]) {
			this.typingUsersByChannel[channelId] = {};
		}

		this.typingUsersByChannel[channelId][userId] = {
			timeout: newTimeout,
			expiresAt: Date.now() + TYPING_TIMEOUT,
		};
	}

	@action
	stopTyping(channelId: string, userId: string): void {
		const channelUsers = this.typingUsersByChannel[channelId];
		if (!channelUsers?.[userId]) {
			return;
		}

		clearTimeout(channelUsers[userId].timeout);

		delete this.typingUsersByChannel[channelId][userId];

		if (Object.keys(this.typingUsersByChannel[channelId]).length === 0) {
			delete this.typingUsersByChannel[channelId];
		}
	}

	stopTypingOnMessageCreate(message: Message): void {
		this.stopTyping(message.channel_id, message.author.id);
	}

	private scheduleClear(channelId: string, userId: string): NodeJS.Timeout {
		return setTimeout(() => this.stopTyping(channelId, userId), TYPING_TIMEOUT);
	}

	private clearAllTimeouts(): void {
		for (const channelUsers of Object.values(this.typingUsersByChannel)) {
			for (const user of Object.values(channelUsers)) {
				clearTimeout(user.timeout);
			}
		}
	}
}

export default new TypingStore();
