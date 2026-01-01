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
import type {StatusType} from '~/Constants';
import {StatusTypes} from '~/Constants';
import type {CustomStatus, GatewayCustomStatusPayload} from '~/lib/customStatus';
import {fromGatewayCustomStatus} from '~/lib/customStatus';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import ConnectionStore from './ConnectionStore';
import GuildMemberStore from './GuildMemberStore';

interface MemberListGroup {
	id: string;
	count: number;
}

interface MemberListItem {
	type: 'member' | 'group';
	data: GuildMemberRecord | MemberListGroup;
}

interface MemberListState {
	memberCount: number;
	onlineCount: number;
	groups: Array<MemberListGroup>;
	items: Map<number, MemberListItem>;
	subscribedRanges: Array<[number, number]>;
	presences: Map<string, StatusType>;
	customStatuses: Map<string, CustomStatus | null>;
}

interface MemberListOperation {
	op: 'SYNC' | 'INSERT' | 'UPDATE' | 'DELETE' | 'INVALIDATE';
	range?: [number, number];
	items?: Array<{
		member?: {
			user: {id: string};
			presence?: {status?: string; custom_status?: GatewayCustomStatusPayload | null} | null;
		};
		group?: MemberListGroup;
	}>;
	index?: number;
	item?: {
		member?: {
			user: {id: string};
			presence?: {status?: string; custom_status?: GatewayCustomStatusPayload | null} | null;
		};
		group?: MemberListGroup;
	};
}

const MEMBER_LIST_TTL_MS = 5 * 60 * 1000;
const MEMBER_LIST_PRUNE_INTERVAL_MS = 30 * 1000;

function areRangesEqual(left?: Array<[number, number]>, right?: Array<[number, number]>): boolean {
	const leftRanges = left ?? [];
	const rightRanges = right ?? [];

	if (leftRanges.length !== rightRanges.length) {
		return false;
	}

	for (let i = 0; i < leftRanges.length; i++) {
		const [leftStart, leftEnd] = leftRanges[i];
		const [rightStart, rightEnd] = rightRanges[i];
		if (leftStart !== rightStart || leftEnd !== rightEnd) {
			return false;
		}
	}

	return true;
}

function removeMemberFromItems(items: Map<number, MemberListItem>, memberId: string): Map<number, MemberListItem> {
	let foundIndex: number | null = null;
	for (const [index, item] of items) {
		if (item.type === 'member') {
			const member = item.data as GuildMemberRecord;
			if (member.user.id === memberId) {
				foundIndex = index;
				break;
			}
		}
	}
	if (foundIndex === null) {
		return items;
	}
	const result = new Map<number, MemberListItem>();
	for (const [index, existingItem] of items) {
		if (index < foundIndex) {
			result.set(index, existingItem);
		} else if (index > foundIndex) {
			result.set(index - 1, existingItem);
		}
	}
	return result;
}

function removePresenceForItem(item: MemberListItem | undefined, presences: Map<string, StatusType>): void {
	if (!item) {
		return;
	}
	if (item.type === 'member') {
		const member = item.data as GuildMemberRecord;
		presences.delete(member.user.id);
	}
}

function removeCustomStatusForItem(
	item: MemberListItem | undefined,
	customStatuses: Map<string, CustomStatus | null>,
): void {
	if (!item || item.type !== 'member') {
		return;
	}
	const member = item.data as GuildMemberRecord;
	customStatuses.delete(member.user.id);
}

class MemberSidebarStore {
	lists: Record<string, Record<string, MemberListState>> = {};
	channelListIds: Record<string, Record<string, string>> = {};
	lastAccess: Record<string, Record<string, number>> = {};
	pruneIntervalId: number | null = null;
	sessionVersion = 0;

	constructor() {
		makeAutoObservable(this, {lastAccess: false, pruneIntervalId: false}, {autoBind: true});
		this.startPruneInterval();
	}

	handleSessionInvalidated(): void {
		this.lists = {};
		this.channelListIds = {};
		this.lastAccess = {};
		this.sessionVersion += 1;
	}

	handleGuildDelete(guildId: string): void {
		if (this.lists[guildId]) {
			const {[guildId]: _, ...remainingLists} = this.lists;
			this.lists = remainingLists;
		}
		if (this.channelListIds[guildId]) {
			const {[guildId]: _, ...remainingMappings} = this.channelListIds;
			this.channelListIds = remainingMappings;
		}
		if (this.lastAccess[guildId]) {
			const {[guildId]: _, ...remainingAccess} = this.lastAccess;
			this.lastAccess = remainingAccess;
		}
	}

	handleGuildCreate(guildId: string): void {
		if (this.lists[guildId]) {
			const {[guildId]: _, ...remainingLists} = this.lists;
			this.lists = remainingLists;
		}
		if (this.channelListIds[guildId]) {
			const {[guildId]: _, ...remainingMappings} = this.channelListIds;
			this.channelListIds = remainingMappings;
		}
		if (this.lastAccess[guildId]) {
			const {[guildId]: _, ...remainingAccess} = this.lastAccess;
			this.lastAccess = remainingAccess;
		}
	}

	handleListUpdate(params: {
		guildId: string;
		listId: string;
		channelId?: string;
		memberCount: number;
		onlineCount: number;
		groups: Array<MemberListGroup>;
		ops: Array<MemberListOperation>;
	}): void {
		const {guildId, listId, channelId, memberCount, onlineCount, groups, ops} = params;
		const storageKey = listId;
		const existingGuildLists = this.lists[guildId] ?? {};
		const guildLists: Record<string, MemberListState> = {...existingGuildLists};

		if (channelId) {
			this.registerChannelListId(guildId, channelId, listId);
			if (guildLists[channelId] && !guildLists[storageKey]) {
				guildLists[storageKey] = guildLists[channelId];
				delete guildLists[channelId];
			}
		}

		if (!guildLists[storageKey]) {
			guildLists[storageKey] = {
				memberCount: 0,
				onlineCount: 0,
				groups: [],
				items: new Map(),
				subscribedRanges: [],
				presences: new Map(),
				customStatuses: new Map(),
			};
		}

		const listState = guildLists[storageKey];
		const newItems = new Map(listState.items);
		const newPresences = new Map(listState.presences);
		const newCustomStatuses = new Map(listState.customStatuses);

		this.touchList(guildId, storageKey);

		for (const op of ops) {
			switch (op.op) {
				case 'SYNC': {
					if (op.range && op.items) {
						const [start, end] = op.range;
						for (let i = start; i <= end; i++) {
							removePresenceForItem(newItems.get(i), newPresences);
							removeCustomStatusForItem(newItems.get(i), newCustomStatuses);
							newItems.delete(i);
						}
						for (let i = 0; i < op.items.length; i++) {
							const rawItem = op.items[i];
							const item = this.convertItem(guildId, rawItem);
							if (item) {
								newItems.set(start + i, item);
								this.extractPresence(rawItem, newPresences);
								this.extractCustomStatus(rawItem, newCustomStatuses);
							}
						}
					}
					break;
				}
				case 'INSERT': {
					if (op.index !== undefined && op.item) {
						const item = this.convertItem(guildId, op.item);
						if (item) {
							if (item.type === 'member') {
								const member = item.data as GuildMemberRecord;
								newPresences.delete(member.user.id);
								newCustomStatuses.delete(member.user.id);
								const deduped = removeMemberFromItems(newItems, member.user.id);
								if (deduped !== newItems) {
									newItems.clear();
									for (const [k, v] of deduped) {
										newItems.set(k, v);
									}
								}
							}
							this.extractPresence(op.item, newPresences);
							this.extractCustomStatus(op.item, newCustomStatuses);
							const shiftedItems = new Map<number, MemberListItem>();
							for (const [index, existingItem] of newItems) {
								if (index >= op.index) {
									shiftedItems.set(index + 1, existingItem);
								} else {
									shiftedItems.set(index, existingItem);
								}
							}
							shiftedItems.set(op.index, item);
							newItems.clear();
							for (const [k, v] of shiftedItems) {
								newItems.set(k, v);
							}
						}
					}
					break;
				}
				case 'UPDATE': {
					if (op.index !== undefined && op.item) {
						removePresenceForItem(newItems.get(op.index), newPresences);
						removeCustomStatusForItem(newItems.get(op.index), newCustomStatuses);
						const item = this.convertItem(guildId, op.item);
						if (item) {
							newItems.set(op.index, item);
							this.extractPresence(op.item, newPresences);
							this.extractCustomStatus(op.item, newCustomStatuses);
						}
					}
					break;
				}
				case 'DELETE': {
					if (op.index !== undefined) {
						removePresenceForItem(newItems.get(op.index), newPresences);
						removeCustomStatusForItem(newItems.get(op.index), newCustomStatuses);
						const shiftedItems = new Map<number, MemberListItem>();
						for (const [index, existingItem] of newItems) {
							if (index > op.index) {
								shiftedItems.set(index - 1, existingItem);
							} else if (index !== op.index) {
								shiftedItems.set(index, existingItem);
							}
						}
						newItems.clear();
						for (const [k, v] of shiftedItems) {
							newItems.set(k, v);
						}
					}
					break;
				}
				case 'INVALIDATE': {
					if (op.range) {
						const [start, end] = op.range;
						for (let i = start; i <= end; i++) {
							removePresenceForItem(newItems.get(i), newPresences);
							removeCustomStatusForItem(newItems.get(i), newCustomStatuses);
							newItems.delete(i);
						}
					}
					break;
				}
			}
		}

		listState.memberCount = memberCount;
		listState.onlineCount = onlineCount;
		listState.groups = groups;
		listState.items = newItems;
		listState.presences = newPresences;
		listState.customStatuses = newCustomStatuses;

		this.lists = {...this.lists, [guildId]: {...guildLists, [storageKey]: listState}};
	}

	private convertItem(
		guildId: string,
		rawItem: {
			member?: {
				user: {id: string};
				presence?: {status?: string; custom_status?: GatewayCustomStatusPayload | null} | null;
			};
			group?: MemberListGroup;
		},
	): MemberListItem | null {
		if (rawItem.group) {
			return {
				type: 'group',
				data: rawItem.group,
			};
		}

		if (rawItem.member) {
			const userId = rawItem.member.user.id;
			const member = GuildMemberStore.getMember(guildId, userId);
			if (member) {
				return {
					type: 'member',
					data: member,
				};
			} else {
				console.warn('[MemberSidebarStore] Member not found in store:', {guildId, userId});
			}
		}

		return null;
	}

	private extractPresence(
		rawItem: {
			member?: {
				user: {id: string};
				presence?: {status?: string; custom_status?: GatewayCustomStatusPayload | null} | null;
			};
			group?: MemberListGroup;
		},
		presences: Map<string, StatusType>,
	): void {
		if (!rawItem.member) {
			return;
		}
		const userId = rawItem.member.user.id;
		const rawPresence = rawItem.member.presence;
		if (rawPresence?.status) {
			presences.set(userId, this.normalizeStatus(rawPresence.status));
		} else {
			presences.delete(userId);
		}
	}

	private extractCustomStatus(
		rawItem: {
			member?: {
				user: {id: string};
				presence?: {status?: string; custom_status?: GatewayCustomStatusPayload | null} | null;
			};
			group?: MemberListGroup;
		},
		customStatuses: Map<string, CustomStatus | null>,
	): void {
		if (!rawItem.member || !rawItem.member.presence) {
			return;
		}
		const userId = rawItem.member.user.id;
		const presence = rawItem.member.presence;
		if (!Object.hasOwn(presence, 'custom_status')) {
			customStatuses.delete(userId);
			return;
		}
		const customStatus = fromGatewayCustomStatus(presence.custom_status ?? null);
		customStatuses.set(userId, customStatus);
	}

	private normalizeStatus(status: string): StatusType {
		switch (status.toLowerCase()) {
			case 'online':
				return StatusTypes.ONLINE;
			case 'idle':
				return StatusTypes.IDLE;
			case 'dnd':
				return StatusTypes.DND;
			default:
				return StatusTypes.OFFLINE;
		}
	}

	subscribeToChannel(guildId: string, channelId: string, ranges: Array<[number, number]>): void {
		const storageKey = this.resolveListKey(guildId, channelId);
		const socket = ConnectionStore.socket;

		const existingGuildLists = this.lists[guildId] ?? {};
		const guildLists: Record<string, MemberListState> = {...existingGuildLists};
		const existingList = guildLists[storageKey];
		const shouldSendUpdate = !areRangesEqual(existingList?.subscribedRanges, ranges);

		if (shouldSendUpdate) {
			socket?.updateGuildSubscriptions({
				subscriptions: {
					[guildId]: {
						member_list_channels: {[channelId]: ranges},
					},
				},
			});
		}

		if (!existingList) {
			guildLists[storageKey] = {
				memberCount: 0,
				onlineCount: 0,
				groups: [],
				items: new Map(),
				subscribedRanges: ranges,
				presences: new Map(),
				customStatuses: new Map(),
			};
		} else {
			guildLists[storageKey] = {...existingList, subscribedRanges: ranges};
		}

		this.touchList(guildId, storageKey);
		this.lists = {...this.lists, [guildId]: guildLists};
	}

	unsubscribeFromChannel(guildId: string, channelId: string): void {
		const socket = ConnectionStore.socket;
		socket?.updateGuildSubscriptions({
			subscriptions: {
				[guildId]: {
					member_list_channels: {[channelId]: []},
				},
			},
		});

		const storageKey = this.resolveListKey(guildId, channelId);
		const existingGuildLists = this.lists[guildId] ?? {};
		const existingList = existingGuildLists[storageKey];

		if (existingList) {
			const guildLists = {...existingGuildLists};
			guildLists[storageKey] = {...existingList, subscribedRanges: []};
			this.lists = {...this.lists, [guildId]: guildLists};
		}
	}

	getSubscribedRanges(guildId: string, channelId: string): Array<[number, number]> {
		const storageKey = this.resolveListKey(guildId, channelId);
		return this.lists[guildId]?.[storageKey]?.subscribedRanges ?? [];
	}

	getVisibleItems(guildId: string, listId: string, range: [number, number]): Array<MemberListItem> {
		const storageKey = this.resolveListKey(guildId, listId);
		const listState = this.lists[guildId]?.[storageKey];
		if (!listState) {
			return [];
		}
		this.touchList(guildId, storageKey);

		const [start, end] = range;
		const items: Array<MemberListItem> = [];

		for (let i = start; i <= end; i++) {
			const item = listState.items.get(i);
			if (item) {
				items.push(item);
			}
		}

		return items;
	}

	getList(guildId: string, listId: string): MemberListState | undefined {
		const storageKey = this.resolveListKey(guildId, listId);
		const list = this.lists[guildId]?.[storageKey];
		if (list) {
			this.touchList(guildId, storageKey);
		}
		return list;
	}

	getMemberCount(guildId: string, listId: string): number {
		const storageKey = this.resolveListKey(guildId, listId);
		return this.lists[guildId]?.[storageKey]?.memberCount ?? 0;
	}

	getOnlineCount(guildId: string, listId: string): number {
		const storageKey = this.resolveListKey(guildId, listId);
		return this.lists[guildId]?.[storageKey]?.onlineCount ?? 0;
	}

	getPresence(guildId: string, listId: string, userId: string): StatusType | null {
		const storageKey = this.resolveListKey(guildId, listId);
		const listState = this.lists[guildId]?.[storageKey];
		if (!listState) {
			return null;
		}
		this.touchList(guildId, storageKey);
		return listState.presences.get(userId) ?? null;
	}

	getCustomStatus(guildId: string, listId: string, userId: string): CustomStatus | null | undefined {
		const storageKey = this.resolveListKey(guildId, listId);
		const listState = this.lists[guildId]?.[storageKey];
		if (!listState) {
			return undefined;
		}
		this.touchList(guildId, storageKey);
		if (!listState.customStatuses.has(userId)) {
			return undefined;
		}
		return listState.customStatuses.get(userId) ?? null;
	}

	private touchList(guildId: string, listId: string): void {
		const now = Date.now();
		if (!this.lastAccess[guildId]) {
			this.lastAccess[guildId] = {};
		}
		this.lastAccess[guildId][listId] = now;
	}

	private resolveListKey(guildId: string, listIdOrChannelId: string): string {
		const guildMappings = this.channelListIds[guildId];
		return guildMappings?.[listIdOrChannelId] ?? listIdOrChannelId;
	}

	private registerChannelListId(guildId: string, channelId: string, listId: string): void {
		const guildMappings = this.channelListIds[guildId] ?? {};
		if (guildMappings[channelId] === listId) {
			if (!this.channelListIds[guildId]) {
				this.channelListIds = {...this.channelListIds, [guildId]: guildMappings};
			}
			return;
		}

		this.channelListIds = {
			...this.channelListIds,
			[guildId]: {...guildMappings, [channelId]: listId},
		};
	}

	private startPruneInterval(): void {
		if (this.pruneIntervalId != null) {
			return;
		}
		this.pruneIntervalId = window.setInterval(() => this.pruneExpiredLists(), MEMBER_LIST_PRUNE_INTERVAL_MS);
	}

	private pruneExpiredLists(): void {
		const now = Date.now();
		const ttlCutoff = now - MEMBER_LIST_TTL_MS;
		const updatedLists: Record<string, Record<string, MemberListState>> = {...this.lists};
		const updatedAccess: Record<string, Record<string, number>> = {...this.lastAccess};
		const updatedMappings: Record<string, Record<string, string>> = {...this.channelListIds};

		Object.entries(this.lastAccess).forEach(([guildId, accessMap]) => {
			const guildLists = {...(updatedLists[guildId] ?? {})};
			const guildAccess = {...accessMap};
			const guildMappings = {...(updatedMappings[guildId] ?? {})};

			Object.entries(accessMap).forEach(([listId, lastSeen]) => {
				if (lastSeen < ttlCutoff) {
					delete guildLists[listId];
					delete guildAccess[listId];

					Object.entries(guildMappings).forEach(([channelId, mappedListId]) => {
						if (mappedListId === listId) {
							delete guildMappings[channelId];
							const socket = ConnectionStore.socket;
							socket?.updateGuildSubscriptions({
								subscriptions: {
									[guildId]: {
										member_list_channels: {[channelId]: []},
									},
								},
							});
						}
					});
				}
			});

			if (Object.keys(guildLists).length === 0) {
				delete updatedLists[guildId];
			} else {
				updatedLists[guildId] = guildLists;
			}

			if (Object.keys(guildAccess).length === 0) {
				delete updatedAccess[guildId];
			} else {
				updatedAccess[guildId] = guildAccess;
			}

			if (Object.keys(guildMappings).length === 0) {
				delete updatedMappings[guildId];
			} else {
				updatedMappings[guildId] = guildMappings;
			}
		});

		this.lists = updatedLists;
		this.lastAccess = updatedAccess;
		this.channelListIds = updatedMappings;
	}
}

export default new MemberSidebarStore();
