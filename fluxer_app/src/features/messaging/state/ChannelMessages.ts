// SPDX-License-Identifier: AGPL-3.0-or-later

import {Message} from '@app/features/messaging/models/MessagingMessage';
import {UploadingAttachment} from '@app/features/messaging/models/UploadingAttachment';
import {resolveChannelIncomingMessageDecision} from '@app/features/messaging/state/ChannelIncomingMessageStateMachine';
import {resolveChannelMessagesLoadDecision} from '@app/features/messaging/state/ChannelMessagesLoadStateMachine';
import MessageReactions from '@app/features/messaging/state/MessageReactions';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import type {JumpType} from '@fluxer/constants/src/JumpConstants';
import {JumpTypes} from '@fluxer/constants/src/JumpConstants';
import {
	MAX_LOADED_MESSAGES,
	MAX_MESSAGE_CACHE_SIZE,
	MAX_MESSAGES_PER_CHANNEL,
	TRUNCATED_MESSAGE_VIEW_SIZE,
} from '@fluxer/constants/src/LimitConstants';
import type {MessageId} from '@fluxer/schema/src/branded/WireIds';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

const IS_MOBILE_CLIENT = /Mobi|Android/i.test(navigator.userAgent);

export interface JumpOptions {
	messageId?: MessageId | null;
	offset?: number;
	present?: boolean;
	flash?: boolean;
	returnToMessageId?: MessageId | null;
	returnChannelId?: string | null;
	returnGuildId?: string | null;
	jumpType?: JumpType;
}

interface JumpToMessageOptions {
	messageId: string;
	flash?: boolean;
	offset?: number;
	returnToMessageId?: MessageId | null;
	returnChannelId?: string | null;
	returnGuildId?: string | null;
	jumpType?: JumpType;
}

interface LoadCompleteOptions {
	windowMessages: Array<MessageInput>;
	isBefore?: boolean;
	isAfter?: boolean;
	jump?: JumpOptions | null;
	hasMoreBefore?: boolean;
	hasMoreAfter?: boolean;
	cached?: boolean;
}

type MessageInput = Message | WireMessage;
type MissingReactionsBehavior = 'empty' | 'preserve';

function toWireMessage(message: MessageInput): WireMessage {
	return message instanceof Message ? message.toJSON() : message;
}

function isUploadPlaceholder(message: Message): boolean {
	return UploadingAttachment.isInSendingMessage(message);
}

function unsentCorrelationKey(message: Message): string {
	return `${message.author.id}\u0000${message.messageReference?.message_id ?? ''}\u0000${message.content}`;
}

function embedsEqualIgnoringId(a: unknown, b: unknown): boolean {
	if (a === b) return true;
	if (a == null || b == null) return a === b;
	if (typeof a !== typeof b) return false;
	if (typeof a !== 'object') return false;
	if (Array.isArray(a)) {
		if (!Array.isArray(b) || a.length !== b.length) return false;
		for (let i = 0; i < a.length; i++) {
			if (!embedsEqualIgnoringId(a[i], b[i])) return false;
		}
		return true;
	}
	if (Array.isArray(b)) return false;
	const aObj = a as Record<string, unknown>;
	const bObj = b as Record<string, unknown>;
	const aKeys = Object.keys(aObj);
	const bKeys = Object.keys(bObj);
	let aRelevant = aKeys.length;
	let bRelevant = bKeys.length;
	if ('id' in aObj) aRelevant--;
	if ('id' in bObj) bRelevant--;
	if (aRelevant !== bRelevant) return false;
	for (const key of aKeys) {
		if (key === 'id') continue;
		if (!(key in bObj)) return false;
		if (!embedsEqualIgnoringId(aObj[key], bObj[key])) return false;
	}
	return true;
}

function shouldUseIncoming(existing: Message, incoming: MessageInput): boolean {
	const previousEdit = existing.editedTimestamp != null ? +existing.editedTimestamp : 0;
	const incomingWire = toWireMessage(incoming);
	const nextEdit = incomingWire.edited_timestamp != null ? +new Date(incomingWire.edited_timestamp) : 0;
	if (nextEdit > previousEdit) return true;
	if (existing.content !== incomingWire.content) return true;
	const existingEmbeds = existing.embeds;
	const incomingEmbeds = incomingWire.embeds;
	const existingHasEmbeds = existingEmbeds && existingEmbeds.length > 0;
	const incomingHasEmbeds = incomingEmbeds && incomingEmbeds.length > 0;
	if (!existingHasEmbeds && !incomingHasEmbeds) return false;
	if (!existingHasEmbeds || !incomingHasEmbeds) return true;
	if (existingEmbeds!.length !== incomingEmbeds!.length) return true;
	for (let i = 0; i < existingEmbeds!.length; i++) {
		if (!embedsEqualIgnoringId(existingEmbeds![i], incomingEmbeds![i])) return true;
	}
	return false;
}

function hydrateMessage(
	channelMessages: ChannelMessages,
	raw: MessageInput,
	missingReactions: MissingReactionsBehavior,
): Message {
	const wire = toWireMessage(raw);
	if ('reactions' in wire) {
		MessageReactions.hydrateMessageReactions(wire.id, wire.reactions);
	} else if (missingReactions === 'empty') {
		MessageReactions.hydrateMessageReactions(wire.id, []);
	}
	const current = channelMessages.get(wire.id);
	if (!current || channelMessages.cached || shouldUseIncoming(current, raw)) {
		return new Message(wire, {missingReactions});
	}
	return current;
}

class MessageBufferSegment {
	private readonly fromOlderSide: boolean;
	private items: Array<Message> = [];
	private keyedById: Record<string, Message> = {};
	private reachedBoundary = false;

	constructor(fromOlderSide: boolean) {
		this.fromOlderSide = fromOlderSide;
	}

	get size(): number {
		return this.items.length;
	}

	get messages(): Array<Message> {
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

	has(id: string): boolean {
		return this.keyedById[id] != null;
	}

	get(id: string): Message | undefined {
		return this.keyedById[id];
	}

	remove(id: string): void {
		if (!this.keyedById[id]) return;
		delete this.keyedById[id];
		this.items = this.items.filter((m) => m.id !== id);
	}

	removeIds(ids: Array<string>): void {
		if (!ids.length) return;
		const idSet = ids.length > 1 ? new Set(ids) : null;
		for (const id of ids) {
			delete this.keyedById[id];
		}
		this.items =
			idSet !== null ? this.items.filter((m) => !idSet.has(m.id)) : this.items.filter((m) => m.id !== ids[0]);
	}

	replace(previousId: string, next: Message): void {
		const existing = this.keyedById[previousId];
		if (!existing) return;
		delete this.keyedById[previousId];
		this.keyedById[next.id] = next;
		const idx = this.items.indexOf(existing);
		if (idx >= 0) this.items[idx] = next;
	}

	update(id: string, updater: (m: Message) => Message): void {
		const current = this.keyedById[id];
		if (!current) return;
		const updated = updater(current);
		this.keyedById[id] = updated;
		const idx = this.items.indexOf(current);
		if (idx >= 0) this.items[idx] = updated;
	}

	forEach(cb: (m: Message, index: number, arr: Array<Message>) => void, thisArg?: unknown): void {
		this.items.forEach(cb, thisArg);
	}

	cache(batch: Array<Message>, boundaryAtInsert = false): void {
		if (batch.length === 0) return;
		if (this.items.length === 0) {
			this.reachedBoundary = boundaryAtInsert;
		}
		if (this.items.length + batch.length > MAX_MESSAGE_CACHE_SIZE) {
			this.reachedBoundary = false;
			if (batch.length >= MAX_MESSAGE_CACHE_SIZE) {
				this.items = this.fromOlderSide
					? batch.slice(batch.length - MAX_MESSAGE_CACHE_SIZE)
					: batch.slice(0, MAX_MESSAGE_CACHE_SIZE);
			} else {
				const available = MAX_MESSAGE_CACHE_SIZE - batch.length;
				const kept = this.fromOlderSide
					? this.items.slice(Math.max(this.items.length - available, 0))
					: this.items.slice(0, available);
				this.items = this.fromOlderSide ? kept.concat(batch) : batch.concat(kept);
			}
			this.keyedById = {};
			for (const msg of this.items) {
				this.keyedById[msg.id] = msg;
			}
			return;
		}
		if (this.fromOlderSide) {
			for (const msg of batch) {
				this.items.push(msg);
				this.keyedById[msg.id] = msg;
			}
			return;
		}
		this.items.unshift(...batch);
		for (const msg of batch) {
			this.keyedById[msg.id] = msg;
		}
	}

	takeAll(): Array<Message> {
		const all = this.items;
		this.items = [];
		this.keyedById = {};
		return all;
	}

	take(count: number): Array<Message> {
		if (count <= 0 || this.items.length === 0) return [];
		let extracted: Array<Message>;
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
	private static readonly channelCache = new Map<string, ChannelMessages>();
	private static readonly maxChannelsInMemory = 50;
	private static readonly retainedChannelIds = new Set<string>();
	private static accessSequence: Set<string> = new Set<string>();
	readonly channelId: string;
	ready = false;
	jumpType: JumpType = JumpTypes.ANIMATED;
	jumpDestinationId: string | null = null;
	jumpDestinationOffset = 0;
	jumpTicket = 1;
	hasJumped = false;
	landedAtLiveEdge = false;
	jumpHighlight = true;
	jumpReturnMessageId: string | null = null;
	jumpReturnChannelId: string | null = null;
	jumpReturnGuildId: string | null = null;
	hasMoreBefore = true;
	hasMoreAfter = false;
	loadingMore = false;
	unblurredMessageId: string | null = null;
	cached = false;
	error = false;
	version = 0;
	private messageList: Array<Message> = [];
	private messageIndex: Record<string, Message> = {};
	private beforeBuffer: MessageBufferSegment;
	private afterBuffer: MessageBufferSegment;

	static forEach(callback: (messages: ChannelMessages, channelId: string) => void): void {
		for (const [id, messages] of ChannelMessages.channelCache) {
			callback(messages, id);
		}
	}

	static get(channelId: string): ChannelMessages | undefined {
		return ChannelMessages.channelCache.get(channelId);
	}

	static hasNewestMessages(channelId: string): boolean {
		return ChannelMessages.get(channelId)?.hasNewestMessages() ?? false;
	}

	static getOrCreate(channelId: string): ChannelMessages {
		let instance = ChannelMessages.channelCache.get(channelId);
		if (!instance) {
			instance = new ChannelMessages(channelId);
			ChannelMessages.channelCache.set(channelId, instance);
			ChannelMessages.evictIfNeeded();
		}
		ChannelMessages.markTouched(channelId);
		return instance;
	}

	static clear(channelId: string): void {
		ChannelMessages.channelCache.delete(channelId);
		ChannelMessages.retainedChannelIds.delete(channelId);
		ChannelMessages.accessSequence.delete(channelId);
	}

	static retainChannel(channelId: string): void {
		ChannelMessages.retainedChannelIds.add(channelId);
		if (ChannelMessages.channelCache.has(channelId)) {
			ChannelMessages.markTouched(channelId);
		}
	}

	static releaseRetainedChannel(channelId: string): void {
		ChannelMessages.retainedChannelIds.delete(channelId);
	}

	static dropBuffers(channelId: string): void {
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

	private static markTouched(channelId: string): void {
		ChannelMessages.accessSequence.delete(channelId);
		ChannelMessages.accessSequence.add(channelId);
	}

	private static sanitizeAccessSequence(): void {
		if (ChannelMessages.accessSequence.size === ChannelMessages.channelCache.size) return;
		for (const channelId of ChannelMessages.accessSequence) {
			if (!ChannelMessages.channelCache.has(channelId)) {
				ChannelMessages.accessSequence.delete(channelId);
			}
		}
	}

	private static evictIfNeeded(): void {
		ChannelMessages.sanitizeAccessSequence();
		while (ChannelMessages.channelCache.size > ChannelMessages.maxChannelsInMemory) {
			const selectedChannelId = SelectedChannel.currentChannelId;
			let evictionCandidate: string | null = null;
			for (const channelId of ChannelMessages.accessSequence) {
				if (channelId === selectedChannelId) continue;
				if (ChannelMessages.retainedChannelIds.has(channelId)) continue;
				evictionCandidate = channelId;
				break;
			}
			if (evictionCandidate !== null) {
				ChannelMessages.accessSequence.delete(evictionCandidate);
				ChannelMessages.channelCache.delete(evictionCandidate);
				continue;
			}
			let didEvict = false;
			for (const channelId of ChannelMessages.channelCache.keys()) {
				if (channelId === selectedChannelId) continue;
				if (ChannelMessages.retainedChannelIds.has(channelId)) continue;
				ChannelMessages.channelCache.delete(channelId);
				ChannelMessages.accessSequence.delete(channelId);
				didEvict = true;
				break;
			}
			if (!didEvict) {
				break;
			}
		}
	}

	constructor(channelId: string) {
		this.channelId = channelId;
		this.beforeBuffer = new MessageBufferSegment(true);
		this.afterBuffer = new MessageBufferSegment(false);
	}

	withPatch(patch: Partial<ChannelMessages>): ChannelMessages {
		return this.cloneAnd(patch);
	}

	get length(): number {
		return this.messageList.length;
	}

	toArray(): Array<Message> {
		return [...this.messageList];
	}

	forEach(
		callback: (message: Message, index: number) => boolean | undefined,
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

	reduce<T>(reducer: (memo: T, message: Message, index: number, array: Array<Message>) => T, initial: T): T {
		return this.messageList.reduce(reducer, initial);
	}

	forEachBuffered(callback: (m: Message, idx: number, arr: Array<Message>) => void, thisArg?: unknown): void {
		this.beforeBuffer.forEach(callback, thisArg);
		this.messageList.forEach(callback, thisArg);
		this.afterBuffer.forEach(callback, thisArg);
	}

	searchFromOldest(predicate: (m: Message) => boolean): Message | undefined {
		return (
			this.beforeBuffer.messages.find(predicate) ??
			this.messageList.find(predicate) ??
			this.afterBuffer.messages.find(predicate)
		);
	}

	searchFromNewest(predicate: (m: Message) => boolean): Message | undefined {
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

	map<T>(mapper: (m: Message, idx: number, arr: Array<Message>) => T, thisArg?: unknown): Array<T> {
		return this.messageList.map(mapper, thisArg);
	}

	first(): Message | undefined {
		return this.messageList[0];
	}

	last(): Message | undefined {
		return this.messageList[this.messageList.length - 1];
	}

	get(id: string, checkBuffers = false): Message | undefined {
		const local = this.messageIndex[id];
		if (local || !checkBuffers) return local;
		return this.beforeBuffer.get(id) ?? this.afterBuffer.get(id);
	}

	atIndex(index: number): Message | undefined {
		return this.messageList[index];
	}

	nextAfter(id: string): Message | null {
		const current = this.get(id);
		if (!current) return null;
		const idx = this.messageList.indexOf(current);
		if (idx < 0 || idx === this.messageList.length - 1) return null;
		return this.messageList[idx + 1] ?? null;
	}

	has(id: string, checkBuffers = true): boolean {
		if (this.messageIndex[id]) return true;
		if (!checkBuffers) return false;
		return this.beforeBuffer.has(id) || this.afterBuffer.has(id);
	}

	indexOf(id: string): number {
		return this.messageList.findIndex((m) => m.id === id);
	}

	hasNewestMessages(): boolean {
		return (this.afterBuffer.size > 0 && this.afterBuffer.isBoundary) || !this.hasMoreAfter;
	}

	canServeOlderFrom(beforeId: string): boolean {
		if (this.messageList.length === 0 || this.beforeBuffer.size === 0) {
			return false;
		}
		const first = this.first();
		return Boolean(first && first.id === beforeId);
	}

	canServeNewerFrom(afterId: string): boolean {
		if (this.messageList.length === 0 || this.afterBuffer.size === 0) {
			return false;
		}
		const last = this.last();
		return Boolean(last && last.id === afterId);
	}

	update(id: string, updater: (m: Message) => Message): ChannelMessages {
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

	replace(previousId: string, next: Message): ChannelMessages {
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

	remove(id: string): ChannelMessages {
		return this.cloneAnd((draft) => {
			delete draft.messageIndex[id];
			draft.messageList = draft.messageList.filter((m) => m.id !== id);
			draft.beforeBuffer.remove(id);
			draft.afterBuffer.remove(id);
		}, true);
	}

	removeIds(ids: Array<string>): ChannelMessages {
		if (!ids.some((id) => this.has(id))) return this;
		const idSet = ids.length > 1 ? new Set(ids) : null;
		return this.cloneAnd((draft) => {
			for (const id of ids) {
				delete draft.messageIndex[id];
			}
			draft.messageList =
				idSet !== null
					? draft.messageList.filter((m) => !idSet.has(m.id))
					: draft.messageList.filter((m) => m.id !== ids[0]);
			draft.beforeBuffer.removeIds(ids);
			draft.afterBuffer.removeIds(ids);
		}, true);
	}

	merge(records: Array<Message>, prepend = false, clearBuffer = false): ChannelMessages {
		return this.cloneAnd((draft) => {
			draft.mergeInto(records, prepend, clearBuffer);
		}, true);
	}

	patchMatching(predicate: (m: Message) => boolean, updater: (m: Message) => Message): ChannelMessages | null {
		let firstChangedIndex = -1;
		let updates: Array<{
			index: number;
			previousId: string;
			record: Message;
		}> | null = null;
		for (let i = 0; i < this.messageList.length; i++) {
			const current = this.messageList[i];
			if (!predicate(current)) continue;
			const next = updater(current);
			if (next === current) continue;
			if (updates === null) {
				updates = [];
				firstChangedIndex = i;
			}
			updates.push({index: i, previousId: current.id, record: next});
		}
		if (updates === null || firstChangedIndex < 0) return null;
		return this.cloneAnd((draft) => {
			for (const {index, previousId, record} of updates!) {
				draft.messageList[index] = record;
				if (record.id !== previousId) {
					delete draft.messageIndex[previousId];
				}
				draft.messageIndex[record.id] = record;
			}
		}, true);
	}

	reset(records: Array<Message>): ChannelMessages {
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

	trimOldest(maxCount: number, deepCopy = true): ChannelMessages {
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

	trimNewest(maxCount: number, deepCopy = true): ChannelMessages {
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

	trimToWindow(trimBottom: boolean, trimTop: boolean): ChannelMessages {
		if (this.length <= MAX_LOADED_MESSAGES) return this;
		if (trimBottom) {
			return this.trimNewest(TRUNCATED_MESSAGE_VIEW_SIZE);
		}
		if (trimTop) {
			return this.trimOldest(TRUNCATED_MESSAGE_VIEW_SIZE);
		}
		return this;
	}

	jumpToLiveEdge(limit: number): ChannelMessages {
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
			draft.hasJumped = true;
			draft.jumpDestinationId = null;
			draft.jumpDestinationOffset = 0;
			draft.landedAtLiveEdge = true;
			draft.jumpHighlight = false;
			draft.jumpReturnMessageId = null;
			draft.jumpReturnChannelId = null;
			draft.jumpReturnGuildId = null;
			draft.jumpTicket += 1;
			draft.ready = true;
			draft.loadingMore = false;
		}, true);
	}

	jumpToMessage({
		messageId,
		flash = true,
		offset,
		returnToMessageId,
		returnChannelId,
		returnGuildId,
		jumpType,
	}: JumpToMessageOptions): ChannelMessages {
		return this.cloneAnd((draft) => {
			draft.hasJumped = true;
			draft.landedAtLiveEdge = false;
			draft.jumpType = jumpType ?? JumpTypes.ANIMATED;
			draft.jumpDestinationId = messageId;
			draft.jumpDestinationOffset = messageId && offset != null ? offset : 0;
			draft.jumpTicket += 1;
			draft.jumpHighlight = flash;
			draft.jumpReturnMessageId = returnToMessageId ?? null;
			draft.jumpReturnChannelId = returnToMessageId ? (returnChannelId ?? draft.channelId) : null;
			draft.jumpReturnGuildId = returnToMessageId ? (returnGuildId ?? null) : null;
			draft.ready = true;
			draft.loadingMore = false;
		}, false);
	}

	clearJumpTarget(options: {clearReturnTarget?: boolean} = {}): ChannelMessages {
		const patch: Partial<ChannelMessages> = {
			hasJumped: false,
			landedAtLiveEdge: false,
			jumpDestinationId: null,
			jumpDestinationOffset: 0,
		};
		if (options.clearReturnTarget) {
			patch.jumpReturnMessageId = null;
			patch.jumpReturnChannelId = null;
			patch.jumpReturnGuildId = null;
		}
		return this.cloneAnd(patch);
	}

	drainBufferedSide(before: boolean, limit: number): ChannelMessages {
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
			next = next.trimToWindow(true, false);
		} else {
			next = next.trimToWindow(false, true);
		}
		return next;
	}

	applyIncomingMessage(message: MessageInput, truncateFromTop = true): ChannelMessages {
		const wire = toWireMessage(message);
		const possibleNonce = wire.nonce ?? null;
		const previous = possibleNonce ? this.get(possibleNonce, true) : null;
		const hasNonceMatch = previous != null && wire.author.id === previous.author.id && previous.id === possibleNonce;
		const decision = resolveChannelIncomingMessageDecision({
			hasNonceMatch,
			isUploadPlaceholder: previous != null && isUploadPlaceholder(previous),
			hasMoreAfter: this.hasMoreAfter,
			afterBufferAtBoundary: this.afterBuffer.isBoundary,
		});
		switch (decision.type) {
			case 'completeUploadPlaceholder': {
				const updated = new Message(wire, {missingReactions: 'preserve'});
				return this.remove(possibleNonce as string).appendIncomingMessage(updated, truncateFromTop);
			}
			case 'replaceNonceMessage': {
				const updated = new Message(wire, {missingReactions: 'preserve'});
				return this.replace(possibleNonce as string, updated);
			}
			case 'ignorePastVisibleWindow':
				if (decision.shouldClearAfterBoundary) {
					this.afterBuffer.isBoundary = false;
				}
				return this;
			case 'appendIncoming':
				return this.appendIncomingMessage(message, truncateFromTop);
		}
	}

	private appendIncomingMessage(message: MessageInput, truncateFromTop: boolean): ChannelMessages {
		const merged = this.merge([hydrateMessage(this, message, 'preserve')]);
		if (truncateFromTop) {
			return merged.trimOldest(IS_MOBILE_CLIENT ? MAX_MESSAGES_PER_CHANNEL : TRUNCATED_MESSAGE_VIEW_SIZE, false);
		}
		if (this.length > MAX_LOADED_MESSAGES) {
			return merged.trimNewest(IS_MOBILE_CLIENT ? MAX_MESSAGES_PER_CHANNEL : TRUNCATED_MESSAGE_VIEW_SIZE, false);
		}
		return merged;
	}

	applyPushPreview(message: MessageInput): ChannelMessages {
		const wire = toWireMessage(message);
		const possibleNonce = wire.nonce ?? null;
		const existing = possibleNonce ? this.get(possibleNonce, true) : null;
		if (existing) return this;
		return this.cloneAnd({ready: true, cached: true}).merge([hydrateMessage(this, wire, 'preserve')]);
	}

	beginLoad(jump?: JumpOptions): ChannelMessages {
		return this.cloneAnd({
			loadingMore: true,
			hasJumped: jump != null,
			landedAtLiveEdge: jump?.present ?? false,
			jumpDestinationId: jump?.messageId ?? null,
			jumpDestinationOffset: jump?.offset ?? 0,
			jumpReturnMessageId: jump?.returnToMessageId ?? null,
			jumpReturnChannelId: jump?.returnToMessageId ? (jump.returnChannelId ?? this.channelId) : null,
			jumpReturnGuildId: jump?.returnToMessageId ? (jump.returnGuildId ?? null) : null,
			ready: jump ? false : this.ready,
		});
	}

	applyLoadedWindow(options: LoadCompleteOptions): ChannelMessages {
		const {
			windowMessages,
			isBefore = false,
			isAfter = false,
			jump = null,
			hasMoreBefore = false,
			hasMoreAfter = false,
			cached = false,
		} = options;
		const records = [...windowMessages].reverse().map((m) => hydrateMessage(this, m, 'empty'));
		const loadDecision = resolveChannelMessagesLoadDecision({
			isBefore,
			isAfter,
			hasJump: jump != null,
			wasReady: this.ready,
		});
		let next: ChannelMessages;
		if (loadDecision.mode === 'replace') {
			const isWindowReplacement = !isBefore && !isAfter && jump?.messageId == null && jump?.offset == null;
			const unsent = isWindowReplacement ? this.collectUnsentMessages(records) : null;
			next = this.reset(records);
			if (unsent && unsent.length > 0) {
				next = next.merge(unsent);
			}
		} else {
			next = this.merge(records, loadDecision.prepend, true);
			if (loadDecision.trimBottom) {
				next = next.trimToWindow(true, false);
			} else if (loadDecision.trimTop) {
				next = next.trimToWindow(false, true);
			}
		}
		next = next.cloneAnd({
			ready: true,
			loadingMore: false,
			jumpType: jump?.jumpType ?? JumpTypes.ANIMATED,
			jumpHighlight: jump?.flash ?? false,
			hasJumped: jump != null,
			landedAtLiveEdge: jump?.present ?? false,
			jumpDestinationId: jump?.messageId ?? null,
			jumpDestinationOffset: jump && jump.messageId != null && jump.offset != null ? jump.offset : 0,
			jumpTicket: jump ? next.jumpTicket + 1 : next.jumpTicket,
			jumpReturnMessageId: jump?.returnToMessageId ?? null,
			jumpReturnChannelId: jump?.returnToMessageId ? (jump.returnChannelId ?? this.channelId) : null,
			jumpReturnGuildId: jump?.returnToMessageId ? (jump.returnGuildId ?? null) : null,
			hasMoreBefore: loadDecision.preserveHasMoreBefore ? next.hasMoreBefore : hasMoreBefore,
			hasMoreAfter: loadDecision.preserveHasMoreAfter ? next.hasMoreAfter : hasMoreAfter,
			cached,
			error: false,
		});
		return next;
	}

	private collectUnsentMessages(records: Array<Message>): Array<Message> {
		const unsent = this.messageList.filter((message) => message.hasFailed || message.isSending);
		if (unsent.length === 0) return unsent;
		const unseenByKey = new Map<string, number>();
		for (const record of records) {
			if (this.has(record.id)) continue;
			const key = unsentCorrelationKey(record);
			unseenByKey.set(key, (unseenByKey.get(key) ?? 0) + 1);
		}
		if (unseenByKey.size === 0) return unsent;
		return unsent.filter((message) => {
			const key = unsentCorrelationKey(message);
			const available = unseenByKey.get(key) ?? 0;
			if (available === 0) return true;
			unseenByKey.set(key, available - 1);
			return false;
		});
	}

	private clearAllMessages(): void {
		this.messageList = [];
		this.messageIndex = {};
	}

	private mergeInto(incoming: Array<Message>, prepend = false, clearSideBuffer = false): void {
		const newItems: Array<Message> = [];
		for (const msg of incoming) {
			const existing = this.messageIndex[msg.id];
			if (existing) {
				const idx = this.messageList.indexOf(existing);
				if (idx >= 0) this.messageList[idx] = msg;
				this.messageIndex[msg.id] = msg;
				continue;
			}
			if (this.beforeBuffer.has(msg.id)) {
				this.beforeBuffer.remove(msg.id);
			} else if (this.afterBuffer.has(msg.id)) {
				this.afterBuffer.remove(msg.id);
			}
			this.messageIndex[msg.id] = msg;
			newItems.push(msg);
		}
		if (clearSideBuffer) {
			const buffer = prepend ? this.beforeBuffer : this.afterBuffer;
			buffer.clear();
		}
		if (newItems.length === 0) return;
		this.messageList = prepend ? newItems.concat(this.messageList) : this.messageList.concat(newItems);
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
			clone.jumpDestinationId = this.jumpDestinationId;
			clone.jumpDestinationOffset = this.jumpDestinationOffset;
			clone.jumpTicket = this.jumpTicket;
			clone.hasJumped = this.hasJumped;
			clone.landedAtLiveEdge = this.landedAtLiveEdge;
			clone.jumpHighlight = this.jumpHighlight;
			clone.jumpReturnMessageId = this.jumpReturnMessageId;
			clone.jumpReturnChannelId = this.jumpReturnChannelId;
			clone.jumpReturnGuildId = this.jumpReturnGuildId;
			clone.hasMoreBefore = this.hasMoreBefore;
			clone.hasMoreAfter = this.hasMoreAfter;
			clone.loadingMore = this.loadingMore;
			clone.unblurredMessageId = this.unblurredMessageId;
			clone.cached = this.cached;
			clone.error = this.error;
			mutator(clone);
		} else {
			const patch = mutator as Partial<ChannelMessages>;
			clone.ready = 'ready' in patch ? !!patch.ready : this.ready;
			clone.jumpType = patch.jumpType ?? this.jumpType;
			clone.jumpDestinationId =
				'jumpDestinationId' in patch ? (patch.jumpDestinationId ?? null) : this.jumpDestinationId;
			clone.jumpDestinationOffset =
				patch.jumpDestinationOffset !== undefined ? patch.jumpDestinationOffset : this.jumpDestinationOffset;
			clone.jumpTicket = patch.jumpTicket !== undefined ? patch.jumpTicket : this.jumpTicket;
			clone.hasJumped = 'hasJumped' in patch ? !!patch.hasJumped : this.hasJumped;
			clone.landedAtLiveEdge = 'landedAtLiveEdge' in patch ? !!patch.landedAtLiveEdge : this.landedAtLiveEdge;
			clone.jumpHighlight = 'jumpHighlight' in patch ? !!patch.jumpHighlight : this.jumpHighlight;
			clone.jumpReturnMessageId =
				'jumpReturnMessageId' in patch ? (patch.jumpReturnMessageId ?? null) : this.jumpReturnMessageId;
			clone.jumpReturnChannelId =
				'jumpReturnChannelId' in patch ? (patch.jumpReturnChannelId ?? null) : this.jumpReturnChannelId;
			clone.jumpReturnGuildId =
				'jumpReturnGuildId' in patch ? (patch.jumpReturnGuildId ?? null) : this.jumpReturnGuildId;
			clone.hasMoreBefore = 'hasMoreBefore' in patch ? !!patch.hasMoreBefore : this.hasMoreBefore;
			clone.hasMoreAfter = 'hasMoreAfter' in patch ? !!patch.hasMoreAfter : this.hasMoreAfter;
			clone.loadingMore = patch.loadingMore !== undefined ? patch.loadingMore : this.loadingMore;
			clone.unblurredMessageId =
				'unblurredMessageId' in patch ? (patch.unblurredMessageId ?? null) : this.unblurredMessageId;
			clone.cached = patch.cached ?? this.cached;
			clone.error = patch.error ?? this.error;
		}
		clone.version = this.version + 1;
		return clone;
	}
}
