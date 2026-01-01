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

import {
	JumpTypes,
	MAX_LOADED_MESSAGES,
	MAX_MESSAGE_CACHE_SIZE,
	MAX_MESSAGES_PER_CHANNEL,
	TRUNCATED_MESSAGE_VIEW_SIZE,
} from '~/Constants';
import {type Message, MessageRecord} from '~/records/MessageRecord';

type MessageId = string;
type ChannelId = string;

const IS_MOBILE_CLIENT = /Mobi|Android/i.test(navigator.userAgent);

export interface JumpOptions {
	messageId?: MessageId | null;
	offset?: number;
	present?: boolean;
	flash?: boolean;
	returnMessageId?: MessageId | null;
	jumpType?: JumpTypes;
}

interface LoadCompleteOptions {
	newMessages: Array<Message>;
	isBefore?: boolean;
	isAfter?: boolean;
	jump?: JumpOptions | null;
	hasMoreBefore?: boolean;
	hasMoreAfter?: boolean;
	cached?: boolean;
}

function shouldUseIncoming(existing: MessageRecord, incoming: Message): boolean {
	const previousEdit = existing.editedTimestamp != null ? +existing.editedTimestamp : 0;
	const nextEdit = incoming.edited_timestamp != null ? +new Date(incoming.edited_timestamp) : 0;

	if (nextEdit > previousEdit) return true;
	if (existing.embeds.length < (incoming.embeds?.length ?? 0)) return true;
	if (existing.content !== incoming.content) return true;

	return false;
}

function hydrateMessage(channelMessages: ChannelMessages, raw: Message): MessageRecord {
	const current = channelMessages.get(raw.id);
	if (!current || channelMessages.cached || shouldUseIncoming(current, raw)) {
		return new MessageRecord(raw);
	}
	return current;
}

class MessageBufferSegment {
	private readonly fromOlderSide: boolean;
	private items: Array<MessageRecord> = [];
	private keyedById: Record<MessageId, MessageRecord> = {};
	private reachedBoundary = false;

	constructor(fromOlderSide: boolean) {
		this.fromOlderSide = fromOlderSide;
	}

	get size(): number {
		return this.items.length;
	}

	get messages(): Array<MessageRecord> {
		return this.items;
	}

	get isBoundary(): boolean {
		return this.reachedBoundary;
	}

	set isBoundary(value: boolean) {
		this.reachedBoundary = value;
	}

	clone(): MessageBufferSegment {
		const clone = new MessageBufferSegment(this.fromOlderSide);
		clone.items = [...this.items];
		clone.keyedById = {...this.keyedById};
		clone.reachedBoundary = this.reachedBoundary;
		return clone;
	}

	clear(): void {
		this.items = [];
		this.keyedById = {};
		this.reachedBoundary = false;
	}

	has(id: MessageId): boolean {
		return this.keyedById[id] != null;
	}

	get(id: MessageId): MessageRecord | undefined {
		return this.keyedById[id];
	}

	remove(id: MessageId): void {
		if (!this.keyedById[id]) return;
		delete this.keyedById[id];
		this.items = this.items.filter((m) => m.id !== id);
	}

	removeMany(ids: Array<MessageId>): void {
		if (!ids.length) return;

		for (const id of ids) {
			delete this.keyedById[id];
		}

		this.items = this.items.filter((m) => !ids.includes(m.id));
	}

	replace(previousId: MessageId, next: MessageRecord): void {
		const existing = this.keyedById[previousId];
		if (!existing) return;

		delete this.keyedById[previousId];
		this.keyedById[next.id] = next;

		const idx = this.items.indexOf(existing);
		if (idx >= 0) this.items[idx] = next;
	}

	update(id: MessageId, updater: (m: MessageRecord) => MessageRecord): void {
		const current = this.keyedById[id];
		if (!current) return;

		const updated = updater(current);
		this.keyedById[id] = updated;

		const idx = this.items.indexOf(current);
		if (idx >= 0) this.items[idx] = updated;
	}

	forEach(cb: (m: MessageRecord, index: number, arr: Array<MessageRecord>) => void, thisArg?: unknown): void {
		this.items.forEach(cb, thisArg);
	}

	cache(batch: Array<MessageRecord>, boundaryAtInsert = false): void {
		if (this.items.length === 0) {
			this.reachedBoundary = boundaryAtInsert;
		}

		const combinedSize = this.items.length + batch.length;

		if (combinedSize > MAX_MESSAGE_CACHE_SIZE) {
			this.reachedBoundary = false;

			if (batch.length >= MAX_MESSAGE_CACHE_SIZE) {
				this.items = this.fromOlderSide
					? batch.slice(batch.length - MAX_MESSAGE_CACHE_SIZE)
					: batch.slice(0, MAX_MESSAGE_CACHE_SIZE);
			} else {
				const available = MAX_MESSAGE_CACHE_SIZE - batch.length;
				this.items = this.fromOlderSide
					? this.items.slice(Math.max(this.items.length - available, 0))
					: this.items.slice(0, available);
			}
		}

		this.items = this.fromOlderSide ? [...this.items, ...batch] : [...batch, ...this.items];

		this.keyedById = {};
		for (const msg of this.items) {
			this.keyedById[msg.id] = msg;
		}
	}

	takeAll(): Array<MessageRecord> {
		const all = this.items;
		this.items = [];
		this.keyedById = {};
		return all;
	}

	take(count: number): Array<MessageRecord> {
		if (count <= 0 || this.items.length === 0) return [];

		let extracted: Array<MessageRecord>;

		if (this.fromOlderSide) {
			const start = Math.max(this.items.length - count, 0);
			extracted = this.items.slice(start);
			this.items.splice(start);
		} else {
			const end = Math.min(count, this.items.length);
			extracted = this.items.slice(0, end);
			this.items.splice(0, end);
		}

		for (const msg of extracted) {
			delete this.keyedById[msg.id];
		}

		return extracted;
	}
}

export class ChannelMessages {
	private static readonly channelCache = new Map<ChannelId, ChannelMessages>();
	private static readonly maxChannelsInMemory = 50;
	private static accessSequence: Array<ChannelId> = [];

	readonly channelId: ChannelId;

	ready = false;

	jumpType: JumpTypes = JumpTypes.ANIMATED;
	jumpTargetId: MessageId | null = null;
	jumpTargetOffset = 0;
	jumpSequenceId = 1;
	jumped = false;
	jumpedToPresent = false;
	jumpFlash = true;
	jumpReturnTargetId: MessageId | null = null;

	hasMoreBefore = true;
	hasMoreAfter = false;
	loadingMore = false;
	revealedMessageId: MessageId | null = null;
	cached = false;
	error = false;
	version = 0;

	private messageList: Array<MessageRecord> = [];
	private messageIndex: Record<MessageId, MessageRecord> = {};
	private beforeBuffer: MessageBufferSegment;
	private afterBuffer: MessageBufferSegment;

	static forEach(callback: (messages: ChannelMessages, channelId: ChannelId) => void): void {
		for (const [id, messages] of ChannelMessages.channelCache) {
			callback(messages, id);
		}
	}

	static get(channelId: ChannelId): ChannelMessages | undefined {
		return ChannelMessages.channelCache.get(channelId);
	}

	static hasPresent(channelId: ChannelId): boolean {
		return ChannelMessages.get(channelId)?.hasPresent() ?? false;
	}

	static getOrCreate(channelId: ChannelId): ChannelMessages {
		let instance = ChannelMessages.channelCache.get(channelId);

		if (!instance) {
			instance = new ChannelMessages(channelId);
			ChannelMessages.channelCache.set(channelId, instance);
			ChannelMessages.evictIfNeeded();
		}

		ChannelMessages.markTouched(channelId);
		return instance;
	}

	static clear(channelId: ChannelId): void {
		ChannelMessages.channelCache.delete(channelId);
		const idx = ChannelMessages.accessSequence.indexOf(channelId);
		if (idx >= 0) ChannelMessages.accessSequence.splice(idx, 1);
	}

	static clearCache(channelId: ChannelId): void {
		const instance = ChannelMessages.channelCache.get(channelId);
		if (!instance) return;

		instance.beforeBuffer.clear();
		instance.afterBuffer.clear();
		ChannelMessages.save(instance);
	}

	static commit(instance: ChannelMessages): ChannelMessages {
		ChannelMessages.channelCache.set(instance.channelId, instance);
		ChannelMessages.markTouched(instance.channelId);
		return instance;
	}

	static save(instance: ChannelMessages): void {
		ChannelMessages.channelCache.set(instance.channelId, instance);
	}

	private static markTouched(channelId: ChannelId): void {
		const existingIndex = ChannelMessages.accessSequence.indexOf(channelId);
		if (existingIndex >= 0) {
			ChannelMessages.accessSequence.splice(existingIndex, 1);
		}
		ChannelMessages.accessSequence.push(channelId);
	}

	private static evictIfNeeded(): void {
		while (ChannelMessages.channelCache.size > ChannelMessages.maxChannelsInMemory) {
			const oldest = ChannelMessages.accessSequence.shift();
			if (oldest) {
				ChannelMessages.channelCache.delete(oldest);
			}
		}
	}

	constructor(channelId: ChannelId) {
		this.channelId = channelId;
		this.beforeBuffer = new MessageBufferSegment(true);
		this.afterBuffer = new MessageBufferSegment(false);
	}

	mutate(patch: Partial<ChannelMessages>): ChannelMessages {
		return this.cloneAnd(patch);
	}

	get length(): number {
		return this.messageList.length;
	}

	toArray(): Array<MessageRecord> {
		return [...this.messageList];
	}

	forEach(
		callback: (message: MessageRecord, index: number) => boolean | undefined,
		thisArg?: unknown,
		reverse = false,
	): void {
		if (reverse) {
			for (let i = this.messageList.length - 1; i >= 0; i--) {
				if (callback.call(thisArg, this.messageList[i], i) === false) {
					break;
				}
			}
			return;
		}

		this.messageList.forEach(callback, thisArg);
	}

	reduce<T>(
		reducer: (memo: T, message: MessageRecord, index: number, array: Array<MessageRecord>) => T,
		initial: T,
	): T {
		return this.messageList.reduce(reducer, initial);
	}

	forAll(callback: (m: MessageRecord, idx: number, arr: Array<MessageRecord>) => void, thisArg?: unknown): void {
		this.beforeBuffer.forEach(callback, thisArg);
		this.messageList.forEach(callback, thisArg);
		this.afterBuffer.forEach(callback, thisArg);
	}

	findOldest(predicate: (m: MessageRecord) => boolean): MessageRecord | undefined {
		return (
			this.beforeBuffer.messages.find(predicate) ??
			this.messageList.find(predicate) ??
			this.afterBuffer.messages.find(predicate)
		);
	}

	findNewest(predicate: (m: MessageRecord) => boolean): MessageRecord | undefined {
		const after = this.afterBuffer.messages;
		for (let i = after.length - 1; i >= 0; i--) {
			if (predicate(after[i])) return after[i];
		}

		for (let i = this.messageList.length - 1; i >= 0; i--) {
			if (predicate(this.messageList[i])) return this.messageList[i];
		}

		const before = this.beforeBuffer.messages;
		for (let i = before.length - 1; i >= 0; i--) {
			if (predicate(before[i])) return before[i];
		}

		return undefined;
	}

	map<T>(mapper: (m: MessageRecord, idx: number, arr: Array<MessageRecord>) => T, thisArg?: unknown): Array<T> {
		return this.messageList.map(mapper, thisArg);
	}

	first(): MessageRecord | undefined {
		return this.messageList[0];
	}

	last(): MessageRecord | undefined {
		return this.messageList[this.messageList.length - 1];
	}

	get(id: MessageId, checkBuffers = false): MessageRecord | undefined {
		const local = this.messageIndex[id];
		if (local || !checkBuffers) return local;
		return this.beforeBuffer.get(id) ?? this.afterBuffer.get(id);
	}

	getByIndex(index: number): MessageRecord | undefined {
		return this.messageList[index];
	}

	getAfter(id: MessageId): MessageRecord | null {
		const current = this.get(id);
		if (!current) return null;

		const idx = this.messageList.indexOf(current);
		if (idx < 0 || idx === this.messageList.length - 1) return null;

		return this.messageList[idx + 1] ?? null;
	}

	has(id: MessageId, checkBuffers = true): boolean {
		if (this.messageIndex[id]) return true;
		if (!checkBuffers) return false;
		return this.beforeBuffer.has(id) || this.afterBuffer.has(id);
	}

	indexOf(id: MessageId): number {
		return this.messageList.findIndex((m) => m.id === id);
	}

	hasPresent(): boolean {
		return (this.afterBuffer.size > 0 && this.afterBuffer.isBoundary) || !this.hasMoreAfter;
	}

	hasBeforeCached(beforeId: MessageId): boolean {
		if (this.messageList.length === 0 || this.beforeBuffer.size === 0) {
			return false;
		}
		const first = this.first();
		return Boolean(first && first.id === beforeId);
	}

	hasAfterCached(afterId: MessageId): boolean {
		if (this.messageList.length === 0 || this.afterBuffer.size === 0) {
			return false;
		}
		const last = this.last();
		return Boolean(last && last.id === afterId);
	}

	update(id: MessageId, updater: (m: MessageRecord) => MessageRecord): ChannelMessages {
		const current = this.messageIndex[id];

		if (!current) {
			if (this.beforeBuffer.has(id)) {
				return this.cloneAnd((draft) => draft.beforeBuffer.update(id, updater), true);
			}
			if (this.afterBuffer.has(id)) {
				return this.cloneAnd((draft) => draft.afterBuffer.update(id, updater), true);
			}
			return this;
		}

		const updated = updater(current);

		return this.cloneAnd((draft) => {
			draft.messageIndex[current.id] = updated;
			const idx = draft.messageList.indexOf(current);
			if (idx >= 0) draft.messageList[idx] = updated;
		}, true);
	}

	replace(previousId: MessageId, next: MessageRecord): ChannelMessages {
		const current = this.messageIndex[previousId];

		if (!current) {
			if (this.beforeBuffer.has(previousId)) {
				return this.cloneAnd((draft) => draft.beforeBuffer.replace(previousId, next), true);
			}
			if (this.afterBuffer.has(previousId)) {
				return this.cloneAnd((draft) => draft.afterBuffer.replace(previousId, next), true);
			}
			return this;
		}

		return this.cloneAnd((draft) => {
			delete draft.messageIndex[previousId];
			draft.messageIndex[next.id] = next;

			const idx = draft.messageList.indexOf(current);
			if (idx >= 0) draft.messageList[idx] = next;
		}, true);
	}

	remove(id: MessageId): ChannelMessages {
		return this.cloneAnd((draft) => {
			delete draft.messageIndex[id];
			draft.messageList = draft.messageList.filter((m) => m.id !== id);
			draft.beforeBuffer.remove(id);
			draft.afterBuffer.remove(id);
		}, true);
	}

	removeMany(ids: Array<MessageId>): ChannelMessages {
		if (!ids.some((id) => this.has(id))) return this;

		return this.cloneAnd((draft) => {
			for (const id of ids) {
				delete draft.messageIndex[id];
			}
			draft.messageList = draft.messageList.filter((m) => !ids.includes(m.id));
			draft.beforeBuffer.removeMany(ids);
			draft.afterBuffer.removeMany(ids);
		}, true);
	}

	merge(records: Array<MessageRecord>, prepend = false, clearBuffer = false): ChannelMessages {
		return this.cloneAnd((draft) => {
			draft.mergeInto(records, prepend, clearBuffer);
		}, true);
	}

	reset(records: Array<MessageRecord>): ChannelMessages {
		return this.cloneAnd((draft) => {
			draft.messageList = records;
			draft.messageIndex = {};
			for (const m of records) {
				draft.messageIndex[m.id] = m;
			}
			draft.beforeBuffer.clear();
			draft.afterBuffer.clear();
		}, true);
	}

	truncateTop(maxCount: number, deepCopy = true): ChannelMessages {
		const overflow = this.messageList.length - maxCount;
		if (overflow <= 0) return this;

		return this.cloneAnd((draft) => {
			for (let i = 0; i < overflow; i++) {
				delete draft.messageIndex[draft.messageList[i].id];
			}

			draft.beforeBuffer.cache(draft.messageList.slice(0, overflow), !draft.hasMoreBefore);
			draft.messageList = draft.messageList.slice(overflow);
			draft.hasMoreBefore = true;
		}, deepCopy);
	}

	truncateBottom(maxCount: number, deepCopy = true): ChannelMessages {
		if (this.messageList.length <= maxCount) return this;

		return this.cloneAnd((draft) => {
			for (let i = maxCount; i < this.messageList.length; i++) {
				delete draft.messageIndex[draft.messageList[i].id];
			}

			draft.afterBuffer.cache(draft.messageList.slice(maxCount, this.messageList.length), !draft.hasMoreAfter);
			draft.messageList = draft.messageList.slice(0, maxCount);
			draft.hasMoreAfter = true;
		}, deepCopy);
	}

	truncate(trimBottom: boolean, trimTop: boolean): ChannelMessages {
		if (this.length <= MAX_LOADED_MESSAGES) return this;

		if (trimBottom) {
			return this.truncateBottom(TRUNCATED_MESSAGE_VIEW_SIZE);
		}

		if (trimTop) {
			return this.truncateTop(TRUNCATED_MESSAGE_VIEW_SIZE);
		}

		return this;
	}

	jumpToPresent(limit: number): ChannelMessages {
		return this.cloneAnd((draft) => {
			const allAfter = draft.afterBuffer.takeAll();
			draft.hasMoreAfter = false;

			const startIndex = Math.max(allAfter.length - limit, 0);
			const visible = allAfter.slice(startIndex);
			const remaining = allAfter.slice(0, startIndex);

			draft.beforeBuffer.cache(draft.messageList);
			draft.beforeBuffer.cache(remaining);

			draft.clearAllMessages();
			draft.mergeInto(visible);

			draft.hasMoreBefore = draft.beforeBuffer.size > 0;
			draft.jumped = true;
			draft.jumpTargetId = null;
			draft.jumpTargetOffset = 0;
			draft.jumpedToPresent = true;
			draft.jumpFlash = false;
			draft.jumpReturnTargetId = null;
			draft.jumpSequenceId += 1;
			draft.ready = true;
			draft.loadingMore = false;
		}, true);
	}

	jumpToMessage(
		messageId: MessageId,
		flash = true,
		offset?: number,
		returnTargetId?: MessageId | null,
		jumpType?: JumpTypes,
	): ChannelMessages {
		return this.cloneAnd((draft) => {
			draft.jumped = true;
			draft.jumpedToPresent = false;
			draft.jumpType = jumpType ?? JumpTypes.ANIMATED;
			draft.jumpTargetId = messageId;
			draft.jumpTargetOffset = messageId && offset != null ? offset : 0;
			draft.jumpSequenceId += 1;
			draft.jumpFlash = flash;
			draft.jumpReturnTargetId = returnTargetId ?? null;
			draft.ready = true;
			draft.loadingMore = false;
		}, false);
	}

	loadFromCache(before: boolean, limit: number): ChannelMessages {
		let next = this.cloneAnd((draft) => {
			const buffer = before ? draft.beforeBuffer : draft.afterBuffer;
			draft.mergeInto(buffer.take(limit), before);

			const hasMore = buffer.size > 0 || !buffer.isBoundary;
			if (before) draft.hasMoreBefore = hasMore;
			else draft.hasMoreAfter = hasMore;

			draft.ready = true;
			draft.loadingMore = false;
		}, true);

		if (before) {
			next = next.truncate(true, false);
		} else {
			next = next.truncate(false, true);
		}

		return next;
	}

	receiveMessage(message: Message, truncateFromTop = true): ChannelMessages {
		const possibleNonce = message.nonce ?? null;
		const previous = possibleNonce ? this.get(possibleNonce, true) : null;

		if (
			previous &&
			message.author.id === previous.author.id &&
			message.nonce != null &&
			previous.id === message.nonce
		) {
			const updated = new MessageRecord(message);
			return this.replace(message.nonce, updated);
		}

		if (this.hasMoreAfter) {
			if (this.afterBuffer.isBoundary) {
				this.afterBuffer.isBoundary = false;
			}
			return this;
		}

		const merged = this.merge([hydrateMessage(this, message)]);

		if (truncateFromTop) {
			return merged.truncateTop(IS_MOBILE_CLIENT ? MAX_MESSAGES_PER_CHANNEL : TRUNCATED_MESSAGE_VIEW_SIZE, false);
		}

		if (this.length > MAX_LOADED_MESSAGES) {
			return merged.truncateBottom(IS_MOBILE_CLIENT ? MAX_MESSAGES_PER_CHANNEL : TRUNCATED_MESSAGE_VIEW_SIZE, false);
		}

		return merged;
	}

	receivePushNotification(message: Message): ChannelMessages {
		const possibleNonce = message.nonce ?? null;
		const existing = possibleNonce ? this.get(possibleNonce, true) : null;
		if (existing) return this;

		return this.cloneAnd({ready: true, cached: true}).merge([hydrateMessage(this, message)]);
	}

	loadStart(jump?: JumpOptions): ChannelMessages {
		return this.cloneAnd({
			loadingMore: true,
			jumped: jump != null,
			jumpedToPresent: jump?.present ?? false,
			jumpTargetId: jump?.messageId ?? null,
			jumpTargetOffset: jump?.offset ?? 0,
			jumpReturnTargetId: jump?.returnMessageId ?? null,
			ready: jump ? false : this.ready,
		});
	}

	loadComplete(options: LoadCompleteOptions): ChannelMessages {
		const {
			newMessages,
			isBefore = false,
			isAfter = false,
			jump = null,
			hasMoreBefore = false,
			hasMoreAfter = false,
			cached = false,
		} = options;

		const records = [...newMessages].reverse().map((m) => hydrateMessage(this, m));

		let next: ChannelMessages;

		if ((!isBefore && !isAfter) || jump || !this.ready) {
			next = this.reset(records);
		} else {
			next = this.merge(records, isBefore, true);
			if (isBefore) {
				next = next.truncate(true, false);
			} else if (isAfter) {
				next = next.truncate(false, true);
			}
		}

		next = next.cloneAnd({
			ready: true,
			loadingMore: false,
			jumpType: jump?.jumpType ?? JumpTypes.ANIMATED,
			jumpFlash: jump?.flash ?? false,
			jumped: jump != null,
			jumpedToPresent: jump?.present ?? false,
			jumpTargetId: jump?.messageId ?? null,
			jumpTargetOffset: jump && jump.messageId != null && jump.offset != null ? jump.offset : 0,
			jumpSequenceId: jump ? next.jumpSequenceId + 1 : next.jumpSequenceId,
			jumpReturnTargetId: jump?.returnMessageId ?? null,
			hasMoreBefore: jump == null && isAfter ? next.hasMoreBefore : hasMoreBefore,
			hasMoreAfter: jump == null && isBefore ? next.hasMoreAfter : hasMoreAfter,
			cached,
			error: false,
		});

		return next;
	}

	private clearAllMessages(): void {
		this.messageList = [];
		this.messageIndex = {};
	}

	private mergeInto(incoming: Array<MessageRecord>, prepend = false, clearSideBuffer = false): void {
		const newItems: Array<MessageRecord> = [];

		for (const msg of incoming) {
			const existing = this.messageIndex[msg.id];
			this.messageIndex[msg.id] = msg;

			if (existing) {
				const idx = this.messageList.indexOf(existing);
				if (idx >= 0) this.messageList[idx] = msg;
			} else {
				newItems.push(msg);
			}
		}

		if (clearSideBuffer) {
			const buffer = prepend ? this.beforeBuffer : this.afterBuffer;
			buffer.clear();
		}

		this.messageList = prepend ? [...newItems, ...this.messageList] : [...this.messageList, ...newItems];
	}

	private cloneAnd(
		mutator: ((draft: ChannelMessages) => void) | Partial<ChannelMessages>,
		deepCopyCollections = false,
	): ChannelMessages {
		const clone = new ChannelMessages(this.channelId);

		clone.messageList = deepCopyCollections ? [...this.messageList] : this.messageList;
		clone.messageIndex = deepCopyCollections ? {...this.messageIndex} : this.messageIndex;
		clone.beforeBuffer = deepCopyCollections ? this.beforeBuffer.clone() : this.beforeBuffer;
		clone.afterBuffer = deepCopyCollections ? this.afterBuffer.clone() : this.afterBuffer;
		clone.version = this.version;

		if (typeof mutator === 'function') {
			clone.ready = this.ready;
			clone.jumpType = this.jumpType;
			clone.jumpTargetId = this.jumpTargetId;
			clone.jumpTargetOffset = this.jumpTargetOffset;
			clone.jumpSequenceId = this.jumpSequenceId;
			clone.jumped = this.jumped;
			clone.jumpedToPresent = this.jumpedToPresent;
			clone.jumpFlash = this.jumpFlash;
			clone.jumpReturnTargetId = this.jumpReturnTargetId;
			clone.hasMoreBefore = this.hasMoreBefore;
			clone.hasMoreAfter = this.hasMoreAfter;
			clone.loadingMore = this.loadingMore;
			clone.revealedMessageId = this.revealedMessageId;
			clone.cached = this.cached;
			clone.error = this.error;

			mutator(clone);
		} else {
			const patch = mutator as Partial<ChannelMessages>;

			clone.ready = 'ready' in patch ? !!patch.ready : this.ready;
			clone.jumpType = patch.jumpType ?? this.jumpType;
			clone.jumpTargetId = 'jumpTargetId' in patch ? (patch.jumpTargetId ?? null) : this.jumpTargetId;
			clone.jumpTargetOffset = patch.jumpTargetOffset !== undefined ? patch.jumpTargetOffset : this.jumpTargetOffset;
			clone.jumpSequenceId = patch.jumpSequenceId !== undefined ? patch.jumpSequenceId : this.jumpSequenceId;
			clone.jumped = 'jumped' in patch ? !!patch.jumped : this.jumped;
			clone.jumpedToPresent = 'jumpedToPresent' in patch ? !!patch.jumpedToPresent : this.jumpedToPresent;
			clone.jumpFlash = 'jumpFlash' in patch ? !!patch.jumpFlash : this.jumpFlash;
			clone.jumpReturnTargetId =
				'jumpReturnTargetId' in patch ? (patch.jumpReturnTargetId ?? null) : this.jumpReturnTargetId;
			clone.hasMoreBefore = 'hasMoreBefore' in patch ? !!patch.hasMoreBefore : this.hasMoreBefore;
			clone.hasMoreAfter = 'hasMoreAfter' in patch ? !!patch.hasMoreAfter : this.hasMoreAfter;
			clone.loadingMore = patch.loadingMore !== undefined ? patch.loadingMore : this.loadingMore;
			clone.revealedMessageId =
				'revealedMessageId' in patch ? (patch.revealedMessageId ?? null) : this.revealedMessageId;
			clone.cached = patch.cached ?? this.cached;
			clone.error = patch.error ?? this.error;
		}

		clone.version = this.version + 1;
		return clone;
	}
}
