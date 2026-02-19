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

import type {CustomStatus, GatewayCustomStatusPayload} from '@app/lib/CustomStatus';
import {fromGatewayCustomStatus} from '@app/lib/CustomStatus';
import {CustomStatusEmitter} from '@app/lib/CustomStatusEmitter';
import {Logger} from '@app/lib/Logger';
import type {GuildMemberRecord} from '@app/records/GuildMemberRecord';
import GuildMemberStore from '@app/stores/GuildMemberStore';
import GuildStore from '@app/stores/GuildStore';
import GatewayConnectionStore from '@app/stores/gateway/GatewayConnectionStore';
import WindowStore from '@app/stores/WindowStore';
import {
	buildMemberListLayout,
	getGroupLayoutForRow,
	getTotalMemberCount,
	getTotalRowsFromLayout,
} from '@app/utils/MemberListLayout';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {makeAutoObservable} from 'mobx';

interface MemberListGroup {
	id: string;
	count: number;
}

interface MemberListItem {
	type: 'member';
	data: GuildMemberRecord;
}

interface MemberListState {
	memberCount: number;
	onlineCount: number;
	groups: Array<MemberListGroup>;
	rows: Map<number, MemberListRow>;
	items: Map<number, MemberListItem>;
	subscribedRanges: Array<[number, number]>;
	presences: Map<string, StatusType>;
	customStatuses: Map<string, CustomStatus | null>;
}

interface MemberListRow {
	type: 'group' | 'member';
	group?: MemberListGroup;
	userId?: string;
	presence?: {status?: string; custom_status?: GatewayCustomStatusPayload | null} | null;
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

class MemberSidebarStore {
	private logger = new Logger('MemberSidebarStore');
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
		if (this.isMemberListUpdatesDisabled(guildId)) {
			return;
		}

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
				rows: new Map(),
				items: new Map(),
				subscribedRanges: [],
				presences: new Map(),
				customStatuses: new Map(),
			};
		}

		const listState = guildLists[storageKey];
		const newRows = new Map(listState.rows);
		const changedCustomStatusUserIds = new Set<string>();

		this.touchList(guildId, storageKey);

		for (const op of ops) {
			switch (op.op) {
				case 'SYNC': {
					if (op.range && op.items) {
						const [start, end] = op.range;
						for (let i = start; i <= end; i++) {
							newRows.delete(i);
						}
						let nextIndex = start;
						for (const rawItem of op.items) {
							const row = this.convertRow(rawItem);
							if (row) {
								newRows.set(nextIndex, row);
							}
							nextIndex += 1;
						}
					}
					break;
				}
				case 'INSERT': {
					if (op.index !== undefined && op.item) {
						const shiftedRows = new Map<number, MemberListRow>();
						for (const [index, existingRow] of newRows) {
							if (index >= op.index) {
								shiftedRows.set(index + 1, existingRow);
							} else {
								shiftedRows.set(index, existingRow);
							}
						}

						const row = this.convertRow(op.item);
						if (row) {
							shiftedRows.set(op.index, row);
						}

						newRows.clear();
						for (const [k, v] of shiftedRows) {
							newRows.set(k, v);
						}
					}
					break;
				}
				case 'UPDATE': {
					if (op.index !== undefined && op.item) {
						const row = this.convertRow(op.item);
						if (row) {
							newRows.set(op.index, row);
						} else {
							newRows.delete(op.index);
						}
					}
					break;
				}
				case 'DELETE': {
					if (op.index !== undefined) {
						const shiftedRows = new Map<number, MemberListRow>();
						for (const [index, existingRow] of newRows) {
							if (index > op.index) {
								shiftedRows.set(index - 1, existingRow);
							} else if (index !== op.index) {
								shiftedRows.set(index, existingRow);
							}
						}
						newRows.clear();
						for (const [k, v] of shiftedRows) {
							newRows.set(k, v);
						}
					}
					break;
				}
				case 'INVALIDATE': {
					if (op.range) {
						const [start, end] = op.range;
						for (let i = start; i <= end; i++) {
							newRows.delete(i);
						}
					}
					break;
				}
			}
		}

		const groupLayouts = buildMemberListLayout(groups);
		const totalMembers = Math.max(memberCount, getTotalMemberCount(groups));
		const totalRows = groupLayouts.length > 0 ? getTotalRowsFromLayout(groupLayouts) : totalMembers;
		const boundedRows = new Map<number, MemberListRow>();
		for (const [index, row] of newRows) {
			if (index < 0 || index >= totalRows) {
				continue;
			}
			boundedRows.set(index, row);
		}

		const sortedRows = Array.from(boundedRows.entries()).sort(([left], [right]) => left - right);
		const newItems = new Map<number, MemberListItem>();
		const newPresences = new Map<string, StatusType>();
		const newCustomStatuses = new Map<string, CustomStatus | null>();
		const seenUserIds = new Set<string>();
		const duplicateUserIds: Array<string> = [];

		for (const [rowIndex, row] of sortedRows) {
			if (row.type !== 'member' || !row.userId) {
				continue;
			}

			let memberIndex: number | null = null;
			if (groupLayouts.length === 0) {
				if (rowIndex >= totalMembers) {
					continue;
				}
				memberIndex = rowIndex;
			} else {
				const layout = getGroupLayoutForRow(groupLayouts, rowIndex);
				if (!layout || rowIndex === layout.headerRowIndex) {
					continue;
				}
				const resolvedMemberIndex = layout.memberStartIndex + (rowIndex - layout.headerRowIndex - 1);
				if (resolvedMemberIndex < 0 || resolvedMemberIndex >= totalMembers) {
					continue;
				}
				memberIndex = resolvedMemberIndex;
			}

			if (seenUserIds.has(row.userId)) {
				duplicateUserIds.push(row.userId);
				continue;
			}
			seenUserIds.add(row.userId);

			const memberItem = this.convertItem(guildId, row.userId);
			if (memberItem) {
				newItems.set(memberIndex, memberItem);
			}

			const presenceStatus = this.extractPresenceFromRow(row);
			if (presenceStatus) {
				newPresences.set(row.userId, presenceStatus);
			}

			if (row.presence && Object.hasOwn(row.presence, 'custom_status')) {
				const customStatus = fromGatewayCustomStatus(row.presence.custom_status ?? null);
				newCustomStatuses.set(row.userId, customStatus);
			}
		}

		const previousCustomStatuses = listState.customStatuses;
		const allCustomStatusUserIds = new Set<string>([
			...Array.from(previousCustomStatuses.keys()),
			...Array.from(newCustomStatuses.keys()),
		]);
		for (const userId of allCustomStatusUserIds) {
			const previousCustomStatus = previousCustomStatuses.has(userId)
				? (previousCustomStatuses.get(userId) ?? null)
				: undefined;
			const nextCustomStatus = newCustomStatuses.has(userId) ? (newCustomStatuses.get(userId) ?? null) : undefined;
			if (previousCustomStatus !== nextCustomStatus) {
				changedCustomStatusUserIds.add(userId);
			}
		}

		listState.memberCount = memberCount;
		listState.onlineCount = onlineCount;
		listState.groups = groups;
		listState.rows = boundedRows;
		listState.items = newItems;
		listState.presences = newPresences;
		listState.customStatuses = newCustomStatuses;

		this.lists = {...this.lists, [guildId]: {...guildLists, [storageKey]: listState}};

		if (duplicateUserIds.length > 0) {
			const uniqueDuplicateUserIds = Array.from(new Set(duplicateUserIds));
			this.logger.warn('Duplicate member rows received in list update:', {
				guildId,
				listId: storageKey,
				duplicateCount: uniqueDuplicateUserIds.length,
				userIds: uniqueDuplicateUserIds.slice(0, 25),
			});
		}

		if (changedCustomStatusUserIds.size > 0) {
			queueMicrotask(() => {
				for (const userId of changedCustomStatusUserIds) {
					CustomStatusEmitter.emitMemberListChange(guildId, storageKey, userId);
				}
			});
		}
	}

	private convertRow(rawItem: {
		member?: {
			user: {id: string};
			presence?: {status?: string; custom_status?: GatewayCustomStatusPayload | null} | null;
		};
		group?: MemberListGroup;
	}): MemberListRow | null {
		if (rawItem.group) {
			return {
				type: 'group',
				group: rawItem.group,
			};
		}

		if (!rawItem.member?.user?.id) {
			return null;
		}

		return {
			type: 'member',
			userId: rawItem.member.user.id,
			presence: rawItem.member.presence ?? null,
		};
	}

	private convertItem(guildId: string, userId: string): MemberListItem | null {
		const member = GuildMemberStore.getMember(guildId, userId);
		if (member) {
			return {
				type: 'member',
				data: member,
			};
		}

		this.logger.warn('Member not found in store:', {guildId, userId});
		return null;
	}

	private extractPresenceFromRow(row: MemberListRow): StatusType | null {
		const status = row.presence?.status;
		if (!status) {
			return null;
		}
		return this.normalizeStatus(status);
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
		if (this.isMemberListUpdatesDisabled(guildId)) {
			return;
		}

		const storageKey = this.resolveListKey(guildId, channelId);
		const socket = GatewayConnectionStore.socket;

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
				rows: new Map(),
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
		const socket = GatewayConnectionStore.socket;
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
		return this.lists[guildId]?.[storageKey];
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
		return listState.presences.get(userId) ?? null;
	}

	getCustomStatus(guildId: string, listId: string, userId: string): CustomStatus | null | undefined {
		const storageKey = this.resolveListKey(guildId, listId);
		const listState = this.lists[guildId]?.[storageKey];
		if (!listState) {
			return undefined;
		}
		if (!listState.customStatuses.has(userId)) {
			return undefined;
		}
		return listState.customStatuses.get(userId) ?? null;
	}

	private isMemberListUpdatesDisabled(guildId: string): boolean {
		const guild = GuildStore.getGuild(guildId);
		if (!guild) {
			return false;
		}
		return (guild.disabledOperations & GuildOperations.MEMBER_LIST_UPDATES) !== 0;
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
		if (!WindowStore.focused) {
			return;
		}

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
				const listState = guildLists[listId];
				if (listState && listState.subscribedRanges.length > 0) {
					guildAccess[listId] = now;
					return;
				}

				if (lastSeen < ttlCutoff) {
					delete guildLists[listId];
					delete guildAccess[listId];

					Object.entries(guildMappings).forEach(([channelId, mappedListId]) => {
						if (mappedListId === listId) {
							delete guildMappings[channelId];
							const socket = GatewayConnectionStore.socket;
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
