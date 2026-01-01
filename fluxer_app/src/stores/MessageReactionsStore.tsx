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
import {type UserPartial, UserRecord} from '~/records/UserRecord';
import UserStore from '~/stores/UserStore';
import {getReactionKey, type ReactionEmoji} from '~/utils/ReactionUtils';

type ReactionUsers = Record<string, UserRecord>;

type FetchStatus = 'idle' | 'pending' | 'success' | 'error';

interface Reaction {
	users: ReactionUsers;
	fetchStatus: FetchStatus;
}

type ReactionMap = Record<string, Reaction>;

const createEmptyReaction = (): Reaction => ({
	users: {},
	fetchStatus: 'idle',
});

class MessageReactionsStore {
	reactions: ReactionMap = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getReactionEntry(messageId: string, emoji: ReactionEmoji): Reaction | undefined {
		const reactionKey = getReactionKey(messageId, emoji);
		return this.reactions[reactionKey];
	}

	getReactions(messageId: string, emoji: ReactionEmoji): ReadonlyArray<UserRecord> {
		const entry = this.getReactionEntry(messageId, emoji);
		return entry ? Object.values(entry.users) : [];
	}

	getFetchStatus(messageId: string, emoji: ReactionEmoji): FetchStatus {
		const entry = this.getReactionEntry(messageId, emoji);
		return entry?.fetchStatus ?? 'idle';
	}

	private getOrCreateReactionEntry(messageId: string, emoji: ReactionEmoji): Reaction {
		const key = getReactionKey(messageId, emoji);
		let entry = this.reactions[key];
		if (!entry) {
			entry = createEmptyReaction();
			this.reactions[key] = entry;
		}
		return entry;
	}

	handleConnectionOpen(): void {
		this.reactions = {};
	}

	handleReactionAdd(messageId: string, userId: string, emoji: ReactionEmoji): void {
		const entry = this.getOrCreateReactionEntry(messageId, emoji);

		const user = UserStore.getUser(userId);
		if (!user) return;

		entry.users[userId] = user;
	}

	handleReactionRemove(messageId: string, userId: string, emoji: ReactionEmoji): void {
		const entry = this.getReactionEntry(messageId, emoji);
		if (!entry) return;

		delete entry.users[userId];
	}

	handleReactionRemoveAll(messageId: string): void {
		const keysToDelete: Array<string> = [];
		for (const key of Object.keys(this.reactions)) {
			if (key.startsWith(messageId)) {
				keysToDelete.push(key);
			}
		}
		for (const key of keysToDelete) {
			delete this.reactions[key];
		}
	}

	handleReactionRemoveEmoji(messageId: string, emoji: ReactionEmoji): void {
		const entry = this.getOrCreateReactionEntry(messageId, emoji);
		entry.users = {};
		entry.fetchStatus = 'idle';
	}

	handleFetchPending(messageId: string, emoji: ReactionEmoji): void {
		const entry = this.getOrCreateReactionEntry(messageId, emoji);
		entry.fetchStatus = 'pending';
	}

	handleFetchSuccess(messageId: string, users: ReadonlyArray<UserPartial>, emoji: ReactionEmoji): void {
		const entry = this.getOrCreateReactionEntry(messageId, emoji);

		UserStore.cacheUsers(users.slice());

		entry.users = {};
		for (const userPartial of users) {
			entry.users[userPartial.id] = new UserRecord(userPartial);
		}

		entry.fetchStatus = 'success';
	}

	handleFetchAppend(messageId: string, users: ReadonlyArray<UserPartial>, emoji: ReactionEmoji): void {
		const entry = this.getReactionEntry(messageId, emoji);

		if (!entry) {
			this.handleFetchSuccess(messageId, users, emoji);
			return;
		}

		UserStore.cacheUsers(users.slice());

		for (const userPartial of users) {
			entry.users[userPartial.id] = new UserRecord(userPartial);
		}

		entry.fetchStatus = 'success';
	}

	handleFetchError(messageId: string, emoji: ReactionEmoji): void {
		const entry = this.getOrCreateReactionEntry(messageId, emoji);
		entry.fetchStatus = 'error';
	}
}

export default new MessageReactionsStore();
