// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import Relationships from '@app/features/relationship/state/Relationships';
import {TYPING_ROLLING_EXPIRY_MS} from '@app/features/typing/rolling/TypingSendThrottle';
import Users from '@app/features/user/state/Users';
import {action, computed, type IComputedValue, makeObservable, type ObservableMap, observable} from 'mobx';

type RollingTypingOrigin = 'local' | 'gateway';

type RollingTypingEntry = Readonly<{confirmed: boolean}>;

const LOCAL_ENTRY: RollingTypingEntry = Object.freeze({confirmed: false});
const CONFIRMED_ENTRY: RollingTypingEntry = Object.freeze({confirmed: true});
const EMPTY_TYPING_USER_IDS: ReadonlyArray<string> = Object.freeze([]);

function areTypingUserIdsEqual(left: ReadonlyArray<string>, right: ReadonlyArray<string>): boolean {
	if (left.length !== right.length) return false;
	for (let index = 0; index < left.length; index += 1) {
		if (left[index] !== right[index]) return false;
	}
	return true;
}

function timerKey(channelId: string, userId: string): string {
	return `${channelId}:${userId}`;
}

class RollingTypingStore {
	private readonly entries = observable.map<string, ObservableMap<string, RollingTypingEntry>>(undefined, {
		deep: false,
	});
	private readonly timers = new Map<string, NodeJS.Timeout>();
	private readonly typingUserIdsByChannel = new Map<string, IComputedValue<ReadonlyArray<string>>>();

	constructor() {
		makeObservable<this, 'expire'>(this, {start: action, remove: action, reset: action, expire: action});
	}

	start(channelId: string, userId: string, origin: RollingTypingOrigin): void {
		const key = timerKey(channelId, userId);
		const previousTimer = this.timers.get(key);
		if (previousTimer !== undefined) {
			clearTimeout(previousTimer);
		}
		const timer: NodeJS.Timeout = setTimeout(() => this.expire(channelId, userId, timer), TYPING_ROLLING_EXPIRY_MS);
		this.timers.set(key, timer);
		const confirmed = origin === 'gateway';
		const existingEntries = this.entries.get(channelId);
		const existing = existingEntries?.get(userId);
		if (existing !== undefined && (existing.confirmed || !confirmed)) {
			return;
		}
		const channelEntries = existingEntries ?? observable.map<string, RollingTypingEntry>(undefined, {deep: false});
		if (existingEntries === undefined) {
			this.entries.set(channelId, channelEntries);
		}
		channelEntries.set(userId, confirmed ? CONFIRMED_ENTRY : LOCAL_ENTRY);
	}

	remove(channelId: string, userId: string): void {
		const key = timerKey(channelId, userId);
		const timer = this.timers.get(key);
		if (timer !== undefined) {
			clearTimeout(timer);
			this.timers.delete(key);
		}
		this.deleteEntry(channelId, userId);
	}

	reset(): void {
		for (const timer of this.timers.values()) {
			clearTimeout(timer);
		}
		this.timers.clear();
		this.entries.clear();
		this.typingUserIdsByChannel.clear();
	}

	countTypists(channelId: string): number {
		return this.entries.get(channelId)?.size ?? 0;
	}

	getTypingUserIds(channelId: string): ReadonlyArray<string> {
		let typingUserIds = this.typingUserIdsByChannel.get(channelId);
		if (typingUserIds === undefined) {
			typingUserIds = computed(() => this.collectTypingUserIds(channelId), {equals: areTypingUserIdsEqual});
			this.typingUserIdsByChannel.set(channelId, typingUserIds);
		}
		return typingUserIds.get();
	}

	isTyping(channelId: string, userId: string): boolean {
		return this.entries.get(channelId)?.has(userId) ?? false;
	}

	isConfirmedTyping(channelId: string, userId: string): boolean {
		return this.entries.get(channelId)?.get(userId)?.confirmed ?? false;
	}

	private expire(channelId: string, userId: string, timer: NodeJS.Timeout): void {
		const key = timerKey(channelId, userId);
		if (this.timers.get(key) !== timer) {
			return;
		}
		this.timers.delete(key);
		this.deleteEntry(channelId, userId);
	}

	private deleteEntry(channelId: string, userId: string): void {
		const channelEntries = this.entries.get(channelId);
		if (channelEntries === undefined || !channelEntries.delete(userId)) {
			return;
		}
		if (channelEntries.size === 0) {
			this.entries.delete(channelId);
		}
	}

	private collectTypingUserIds(channelId: string): ReadonlyArray<string> {
		const channelEntries = this.entries.get(channelId);
		if (channelEntries === undefined) {
			return EMPTY_TYPING_USER_IDS;
		}
		const currentUserId = Authentication.currentUserId;
		const showSelf = DeveloperOptions.showMyselfTyping;
		const typingUserIds: Array<string> = [];
		for (const userId of channelEntries.keys()) {
			if (!showSelf && userId === currentUserId) continue;
			if (Relationships.isBlocked(userId)) continue;
			if (Users.getUser(userId) === undefined) continue;
			typingUserIds.push(userId);
		}
		return typingUserIds.length === 0 ? EMPTY_TYPING_USER_IDS : typingUserIds;
	}
}

export default new RollingTypingStore();
