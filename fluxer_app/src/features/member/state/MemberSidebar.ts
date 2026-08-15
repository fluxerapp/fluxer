// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import Channels from '@app/features/channel/state/Channels';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import Guilds from '@app/features/guild/state/Guilds';
import {GuildMember} from '@app/features/member/models/GuildMember';
import {getHydratedMemberListRanges} from '@app/features/member/utils/MemberListHydration';
import {
	buildMemberListLayout,
	getGroupLayoutForRow,
	getTotalMemberCount,
	getTotalRowsFromLayout,
} from '@app/features/member/utils/MemberListLayout';
import {
	areNormalizedMemberListRangesCovered,
	isIndexInMemberListRanges,
	type MemberListRanges,
	type NormalizedMemberListRanges,
	normalizeMemberListRanges,
} from '@app/features/member/utils/MemberListRangeUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {CustomStatus, GatewayCustomStatusPayload} from '@app/features/user/state/CustomStatus';
import {fromGatewayCustomStatus} from '@app/features/user/state/CustomStatus';
import {CustomStatusEmitter} from '@app/features/user/state/CustomStatusEmitter';
import Window from '@app/features/window/state/Window';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {makeAutoObservable, observable} from 'mobx';

interface MemberListGroup {
	id: string;
	count: number;
}

interface MemberListMember {
	userId: string;
	member: MemberListOperationMember;
}

interface MemberListItem {
	type: 'member';
	data: MemberListMember;
}

interface MemberListState {
	hasReceivedInitialPayload: boolean;
	memberCount: number;
	onlineCount: number;
	groups: Array<MemberListGroup>;
	rows: Map<number, MemberListRow>;
	items: Map<number, MemberListItem>;
	requestedRanges: NormalizedMemberListRanges;
	subscribedRanges: NormalizedMemberListRanges;
	presences: Map<string, StatusType>;
	customStatuses: Map<string, CustomStatus | null>;
	knownCustomStatuses: Map<string, CustomStatus | null>;
}

interface MemberListRow {
	type: 'group' | 'member';
	group?: MemberListGroup;
	userId?: string;
	member?: MemberListOperationMember;
	presence?: {
		status?: string;
		custom_status?: GatewayCustomStatusPayload | null;
	} | null;
}

type MemberListOperationMember = GuildMemberData & {
	presence?: {
		status?: string;
		custom_status?: GatewayCustomStatusPayload | null;
	} | null;
};

type MemberListOperationItem = {
	member?: MemberListOperationMember;
	group?: MemberListGroup;
};

interface MemberListOperation {
	op: 'SYNC';
	range: [number, number];
	items: ReadonlyArray<MemberListOperationItem>;
}

interface MemberListUpdateParams {
	guildId: string;
	listId: string;
	channelId?: string;
	memberCount: number;
	onlineCount: number;
	groups: ReadonlyArray<MemberListGroup>;
	ops: ReadonlyArray<MemberListOperation>;
}

interface PendingMemberListUpdateBatch {
	guildId: string;
	listId: string;
	timeoutId: number;
	demand: MemberListUpdateDemand;
	updates: Array<MemberListUpdateParams>;
}

interface MemberListUpdateDemand {
	generation: number;
	guildId: string;
	channelId: string;
	ownerId: string | null;
}

const MEMBER_LIST_TTL_MS = 60 * 1000;
const MEMBER_LIST_CACHE_MAX_LISTS = 8;
const MEMBER_LIST_PRUNE_INTERVAL_MS = 30 * 1000;
const MEMBER_LIST_UPDATE_BATCH_MS = 1000;
const EMPTY_MEMBER_LIST_RANGES = normalizeMemberListRanges([]);

interface MemberListSubscription {
	guildId: string;
	channelId: string;
	ownerId: string | null;
}

function areCustomStatusesEqual(
	left: CustomStatus | null | undefined,
	right: CustomStatus | null | undefined,
): boolean {
	if (left == null || right == null) {
		return left == null && right == null;
	}
	return (
		left.text === right.text &&
		left.expiresAt === right.expiresAt &&
		left.emojiId === right.emojiId &&
		left.emojiName === right.emojiName &&
		(left.emojiAnimated ?? null) === (right.emojiAnimated ?? null)
	);
}

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

class MemberSidebar {
	private logger = new Logger('MemberSidebar');
	lists: Record<string, Record<string, MemberListState>> = {};
	channelListIds: Record<string, Record<string, string>> = {};
	private permissionListIds: Record<string, Record<string, string>> = {};
	private listSubscribedChannelIds: Record<string, Record<string, string>> = {};
	private syncedMemberListGuildIds = new Set<string>();
	private sentMemberListGuildId: string | null = null;
	private sentMemberListChannelId: string | null = null;
	private sentMemberListRanges: NormalizedMemberListRanges = EMPTY_MEMBER_LIST_RANGES;
	private gatewaySessionId: string | null = null;
	private activeMemberListSubscription: MemberListSubscription | null = null;
	lastAccess: Record<string, Record<string, number>> = {};
	pruneIntervalId: number | null = null;
	sessionVersion = 0;
	memberListSubscriptionGeneration = 0;
	private materializedMemberCache = new WeakMap<MemberListMember, GuildMember>();
	private pendingListUpdateBatches = new Map<string, PendingMemberListUpdateBatch>();

	constructor() {
		makeAutoObservable<
			this,
			| 'permissionListIds'
			| 'listSubscribedChannelIds'
			| 'syncedMemberListGuildIds'
			| 'sentMemberListGuildId'
			| 'sentMemberListChannelId'
			| 'sentMemberListRanges'
			| 'gatewaySessionId'
			| 'activeMemberListSubscription'
			| 'lastAccess'
			| 'pruneIntervalId'
			| 'materializedMemberCache'
			| 'pendingListUpdateBatches'
		>(
			this,
			{
				lists: observable.ref,
				permissionListIds: false,
				listSubscribedChannelIds: false,
				syncedMemberListGuildIds: false,
				sentMemberListGuildId: false,
				sentMemberListChannelId: false,
				sentMemberListRanges: false,
				gatewaySessionId: false,
				activeMemberListSubscription: false,
				lastAccess: false,
				pruneIntervalId: false,
				materializedMemberCache: false,
				pendingListUpdateBatches: false,
			},
			{autoBind: true},
		);
		this.startPruneInterval();
	}

	synchronizeGatewaySession(sessionId: string | null): void {
		const previousSessionId = this.gatewaySessionId;
		if (previousSessionId === sessionId) {
			return;
		}
		this.gatewaySessionId = sessionId;
		if (previousSessionId !== null || sessionId === null) {
			this.handleSessionInvalidated();
		}
	}

	handleSessionInvalidated(): void {
		this.clearPendingListUpdateBatches();
		this.lists = {};
		this.channelListIds = {};
		this.permissionListIds = {};
		this.listSubscribedChannelIds = {};
		this.syncedMemberListGuildIds.clear();
		this.clearSentMemberListSubscription();
		this.setActiveMemberListSubscription(null);
		this.lastAccess = {};
		this.sessionVersion += 1;
	}

	handleGuildDelete(guildId: string): void {
		this.clearPendingListUpdateBatches(guildId);
		if (this.lists[guildId]) {
			const {[guildId]: _, ...remainingLists} = this.lists;
			this.lists = remainingLists;
		}
		if (this.channelListIds[guildId]) {
			const {[guildId]: _, ...remainingMappings} = this.channelListIds;
			this.channelListIds = remainingMappings;
		}
		if (this.permissionListIds[guildId]) {
			const {[guildId]: _, ...remainingPermissionMappings} = this.permissionListIds;
			this.permissionListIds = remainingPermissionMappings;
		}
		if (this.listSubscribedChannelIds[guildId]) {
			const {[guildId]: _, ...remainingSubscribedChannels} = this.listSubscribedChannelIds;
			this.listSubscribedChannelIds = remainingSubscribedChannels;
		}
		this.syncedMemberListGuildIds.delete(guildId);
		if (this.sentMemberListGuildId === guildId) {
			this.clearSentMemberListSubscription();
		}
		if (this.activeMemberListSubscription?.guildId === guildId) {
			this.setActiveMemberListSubscription(null);
		}
		if (this.lastAccess[guildId]) {
			const {[guildId]: _, ...remainingAccess} = this.lastAccess;
			this.lastAccess = remainingAccess;
		}
	}

	handleGuildCreate(guildId: string): void {
		this.clearPendingListUpdateBatches(guildId);
		if (this.lists[guildId]) {
			const {[guildId]: _, ...remainingLists} = this.lists;
			this.lists = remainingLists;
		}
		if (this.channelListIds[guildId]) {
			const {[guildId]: _, ...remainingMappings} = this.channelListIds;
			this.channelListIds = remainingMappings;
		}
		if (this.permissionListIds[guildId]) {
			const {[guildId]: _, ...remainingPermissionMappings} = this.permissionListIds;
			this.permissionListIds = remainingPermissionMappings;
		}
		if (this.listSubscribedChannelIds[guildId]) {
			const {[guildId]: _, ...remainingSubscribedChannels} = this.listSubscribedChannelIds;
			this.listSubscribedChannelIds = remainingSubscribedChannels;
		}
		this.syncedMemberListGuildIds.delete(guildId);
		if (this.sentMemberListGuildId === guildId) {
			this.clearSentMemberListSubscription();
		}
		if (this.activeMemberListSubscription?.guildId === guildId) {
			this.setActiveMemberListSubscription(null);
		}
		if (this.lastAccess[guildId]) {
			const {[guildId]: _, ...remainingAccess} = this.lastAccess;
			this.lastAccess = remainingAccess;
		}
	}

	handleListUpdate(params: MemberListUpdateParams): void {
		const {guildId, listId, channelId} = params;
		if (this.isMemberListUpdatesDisabled(guildId)) {
			return;
		}
		const demand = this.getMemberListUpdateDemand(params);
		if (demand == null) {
			return;
		}
		const existingGuildLists = this.lists[guildId] ?? {};
		const localStorageKey = channelId ? this.getLocalChannelListKey(guildId, channelId) : undefined;
		const existingList =
			existingGuildLists[listId] ??
			(localStorageKey ? existingGuildLists[localStorageKey] : undefined) ??
			(channelId ? existingGuildLists[channelId] : undefined);
		if (!existingList?.hasReceivedInitialPayload || typeof window === 'undefined') {
			this.applyListUpdate(params, demand);
			return;
		}
		if (this.shouldBypassListUpdateBatchForCurrentUserPresence(params, existingList)) {
			this.flushPendingListUpdateBatch(this.getListUpdateBatchKey(guildId, listId));
			this.applyListUpdate(params, demand);
			return;
		}
		this.queueListUpdate(params, demand);
	}

	private queueListUpdate(params: MemberListUpdateParams, demand: MemberListUpdateDemand): void {
		if (!this.isMemberListUpdateAccepted(demand, params)) {
			return;
		}
		const {guildId, listId} = params;
		const batchKey = this.getListUpdateBatchKey(guildId, listId);
		let batch = this.pendingListUpdateBatches.get(batchKey);
		if (
			batch != null &&
			(!this.isMemberListUpdateDemandCurrent(batch.demand) ||
				!this.areMemberListUpdateDemandsEqual(batch.demand, demand))
		) {
			window.clearTimeout(batch.timeoutId);
			this.pendingListUpdateBatches.delete(batchKey);
			batch = undefined;
		}
		if (!batch) {
			batch = {
				guildId,
				listId,
				timeoutId: window.setTimeout(() => this.flushPendingListUpdateBatch(batchKey), MEMBER_LIST_UPDATE_BATCH_MS),
				demand,
				updates: [],
			};
			this.pendingListUpdateBatches.set(batchKey, batch);
		}
		if (batch.updates.length === 0) {
			batch.updates.push(params);
		} else {
			batch.updates[0] = this.mergeQueuedListUpdate(batch.updates[0], params);
		}
		this.touchList(guildId, listId);
	}

	private flushPendingListUpdateBatch(batchKey: string): void {
		const batch = this.pendingListUpdateBatches.get(batchKey);
		if (!batch) {
			return;
		}
		this.pendingListUpdateBatches.delete(batchKey);
		window.clearTimeout(batch.timeoutId);
		for (const update of batch.updates) {
			this.applyListUpdate(update, batch.demand);
		}
	}

	private clearPendingListUpdateBatches(guildId?: string): void {
		for (const [batchKey, batch] of Array.from(this.pendingListUpdateBatches.entries())) {
			if (guildId !== undefined && batch.guildId !== guildId) {
				continue;
			}
			window.clearTimeout(batch.timeoutId);
			this.pendingListUpdateBatches.delete(batchKey);
		}
	}

	private clearPendingListUpdateBatch(guildId: string, listId: string): void {
		const candidateListIds = new Set<string>([listId, this.resolveListKey(guildId, listId)]);
		const localStorageKey = this.getLocalChannelListKey(guildId, listId);
		if (localStorageKey) {
			candidateListIds.add(localStorageKey);
		}
		for (const candidateListId of candidateListIds) {
			const batchKey = this.getListUpdateBatchKey(guildId, candidateListId);
			const batch = this.pendingListUpdateBatches.get(batchKey);
			if (!batch) {
				continue;
			}
			window.clearTimeout(batch.timeoutId);
			this.pendingListUpdateBatches.delete(batchKey);
		}
	}

	private getListUpdateBatchKey(guildId: string, listId: string): string {
		return `${guildId}\u0000${listId}`;
	}

	private mergeQueuedListUpdate(
		existing: MemberListUpdateParams,
		next: MemberListUpdateParams,
	): MemberListUpdateParams {
		return {
			...next,
			channelId: next.channelId ?? existing.channelId,
			ops: this.mergeMemberListSyncOps([...existing.ops, ...next.ops]),
		};
	}

	private mergeMemberListSyncOps(ops: Array<MemberListOperation>): Array<MemberListOperation> {
		const mergedOps: Array<MemberListOperation> = [];
		for (const op of ops) {
			let mergedOp = op;
			for (let index = 0; index < mergedOps.length; ) {
				const existing = mergedOps[index];
				if (!this.doMemberListSyncRangesOverlap(existing.range, mergedOp.range)) {
					index += 1;
					continue;
				}
				mergedOp = this.mergeMemberListSyncOp(existing, mergedOp);
				mergedOps.splice(index, 1);
			}
			mergedOps.push(mergedOp);
		}
		mergedOps.sort((left, right) => left.range[0] - right.range[0]);
		return mergedOps;
	}

	private doMemberListSyncRangesOverlap(left: [number, number], right: [number, number]): boolean {
		return left[0] <= right[1] && right[0] <= left[1];
	}

	private mergeMemberListSyncOp(existing: MemberListOperation, next: MemberListOperation): MemberListOperation {
		const start = Math.min(existing.range[0], next.range[0]);
		const end = Math.max(existing.range[1], next.range[1]);
		const rows = new Map<number, MemberListOperationItem>();
		this.applyMemberListSyncOpToRows(rows, existing);
		this.applyMemberListSyncOpToRows(rows, next);
		const items: Array<MemberListOperationItem> = [];
		for (let index = start; index <= end; index += 1) {
			items.push(rows.get(index) ?? {});
		}
		return {
			op: 'SYNC',
			range: [start, end],
			items,
		};
	}

	private applyMemberListSyncOpToRows(rows: Map<number, MemberListOperationItem>, op: MemberListOperation): void {
		const [start, end] = op.range;
		for (let index = start; index <= end; index += 1) {
			rows.delete(index);
		}
		let nextIndex = start;
		for (const item of op.items) {
			if (this.isEmptyGroupItem(item)) {
				continue;
			}
			if (item.member?.user?.id || item.group) {
				rows.set(nextIndex, item);
			} else {
				rows.delete(nextIndex);
			}
			nextIndex += 1;
		}
	}

	private shouldBypassListUpdateBatchForCurrentUserPresence(
		params: MemberListUpdateParams,
		listState: MemberListState,
	): boolean {
		const currentUserId = Authentication.currentUserId;
		if (!currentUserId || listState.requestedRanges.length === 0) {
			return false;
		}
		let currentUserVisible = false;
		for (const item of listState.items.values()) {
			if (item.data.userId === currentUserId) {
				currentUserVisible = true;
				break;
			}
		}
		if (!currentUserVisible) {
			return false;
		}
		for (const op of params.ops) {
			for (const item of op.items) {
				const member = item.member;
				if (member?.user?.id !== currentUserId) {
					continue;
				}
				if (this.memberPresencePayloadChanged(listState, currentUserId, member.presence ?? null)) {
					return true;
				}
			}
		}
		return false;
	}

	private memberPresencePayloadChanged(
		listState: MemberListState,
		userId: string,
		presence: MemberListOperationMember['presence'],
	): boolean {
		if (!presence) {
			return false;
		}
		if (presence.status != null) {
			const nextStatus = this.normalizeStatus(presence.status);
			const currentStatus = listState.presences.get(userId) ?? StatusTypes.OFFLINE;
			if (nextStatus !== currentStatus) {
				return true;
			}
		}
		if (Object.hasOwn(presence, 'custom_status')) {
			const nextCustomStatus = fromGatewayCustomStatus(presence.custom_status ?? null);
			const currentCustomStatus = listState.customStatuses.has(userId)
				? (listState.customStatuses.get(userId) ?? null)
				: (listState.knownCustomStatuses.get(userId) ?? null);
			if (!areCustomStatusesEqual(currentCustomStatus, nextCustomStatus)) {
				return true;
			}
		}
		return false;
	}

	private applyListUpdate(params: MemberListUpdateParams, demand: MemberListUpdateDemand): void {
		if (!this.isMemberListUpdateAccepted(demand, params)) {
			return;
		}
		const {guildId, listId, channelId, memberCount, onlineCount, groups, ops} = params;
		if (this.isMemberListUpdatesDisabled(guildId)) {
			return;
		}
		const storageKey = listId;
		const existingGuildLists = this.lists[guildId] ?? {};
		const guildLists: Record<string, MemberListState> = {...existingGuildLists};
		const localStorageKey = channelId ? this.getLocalChannelListKey(guildId, channelId) : undefined;
		const preRegistrationStorageKey = channelId ? this.resolveListKey(guildId, channelId) : undefined;
		if (channelId) {
			this.registerChannelListId(guildId, channelId, listId);
			for (const aliasKey of [preRegistrationStorageKey, localStorageKey, channelId]) {
				if (!aliasKey || aliasKey === storageKey) {
					continue;
				}
				if (guildLists[aliasKey]) {
					if (!guildLists[storageKey]) {
						guildLists[storageKey] = guildLists[aliasKey];
					}
					delete guildLists[aliasKey];
					this.moveListAccess(guildId, aliasKey, storageKey);
				}
				this.moveListSubscribedChannel(guildId, aliasKey, storageKey);
			}
		}
		if (!guildLists[storageKey]) {
			guildLists[storageKey] = this.createEmptyListState();
		}
		const listState = guildLists[storageKey];
		const newRows = new Map(listState.rows);
		const changedPresenceUserIds = new Set<string>();
		const changedCustomStatusUserIds = new Set<string>();
		this.touchList(guildId, storageKey);
		for (const op of ops) {
			const [start, end] = op.range;
			for (let i = start; i <= end; i++) {
				newRows.delete(i);
			}
			let nextIndex = start;
			for (const rawItem of op.items) {
				const row = this.convertRow(rawItem);
				if (row) {
					newRows.set(nextIndex, row);
					nextIndex += 1;
				} else if (!this.isEmptyGroupItem(rawItem)) {
					nextIndex += 1;
				}
			}
		}
		const visibleGroups = this.visibleGroups(groups);
		const groupLayouts = buildMemberListLayout(visibleGroups);
		const totalMembers = Math.max(memberCount, getTotalMemberCount(visibleGroups));
		const totalRows = groupLayouts.length > 0 ? getTotalRowsFromLayout(groupLayouts) : totalMembers;
		const boundedRows = new Map<number, MemberListRow>();
		for (const [index, row] of newRows) {
			if (index < 0 || index >= totalRows) {
				continue;
			}
			boundedRows.set(index, row);
		}
		const requestedRanges = normalizeMemberListRanges(listState.requestedRanges);
		const prunedRows = requestedRanges.length > 0 ? this.pruneRowsToRanges(boundedRows, requestedRanges) : boundedRows;
		const newItems = new Map<number, MemberListItem>();
		const newPresences = new Map<string, StatusType>();
		const newCustomStatuses = new Map<string, CustomStatus | null>();
		const nextKnownCustomStatuses = new Map(listState.knownCustomStatuses);
		const userIdRowCounts = new Map<string, number>();
		const recordCustomStatus = (userId: string, customStatus: CustomStatus | null) => {
			newCustomStatuses.set(userId, customStatus);
			const hadKnownCustomStatus = nextKnownCustomStatuses.has(userId);
			const previousKnownCustomStatus = hadKnownCustomStatus
				? (nextKnownCustomStatuses.get(userId) ?? null)
				: undefined;
			if (!hadKnownCustomStatus || !areCustomStatusesEqual(previousKnownCustomStatus ?? null, customStatus)) {
				changedCustomStatusUserIds.add(userId);
			}
			nextKnownCustomStatuses.set(userId, customStatus);
		};
		const clearKnownCustomStatus = (userId: string) => {
			if ((nextKnownCustomStatuses.get(userId) ?? null) == null) {
				return;
			}
			recordCustomStatus(userId, null);
		};
		this.visitRowsInIndexOrder(prunedRows, requestedRanges, (rowIndex, row) => {
			if (row.type !== 'member' || !row.userId) {
				return;
			}
			let rowGroupId: string | null = null;
			if (groupLayouts.length > 0) {
				const layout = getGroupLayoutForRow(groupLayouts, rowIndex);
				if (!layout || rowIndex === layout.headerRowIndex) {
					return;
				}
				rowGroupId = layout.id;
			}
			userIdRowCounts.set(row.userId, (userIdRowCounts.get(row.userId) ?? 0) + 1);
			const memberItem = this.convertItem(guildId, row);
			if (memberItem) {
				newItems.set(rowIndex, memberItem);
			}
			const presenceStatus = this.extractPresenceFromRow(row);
			if (presenceStatus) {
				const previousPresenceStatus = listState.presences.get(row.userId);
				if (previousPresenceStatus !== undefined && previousPresenceStatus !== presenceStatus) {
					changedPresenceUserIds.add(row.userId);
				}
				newPresences.set(row.userId, presenceStatus);
			}
			if (row.presence && Object.hasOwn(row.presence, 'custom_status')) {
				const customStatus = fromGatewayCustomStatus(row.presence.custom_status ?? null);
				recordCustomStatus(row.userId, customStatus);
			} else if (presenceStatus === StatusTypes.OFFLINE || (row.presence == null && rowGroupId === 'offline')) {
				clearKnownCustomStatus(row.userId);
			}
		});
		const duplicateUserIds: Array<string> = [];
		for (const [userId, count] of userIdRowCounts) {
			if (count > 1) {
				duplicateUserIds.push(userId);
			}
		}
		listState.memberCount = memberCount;
		listState.onlineCount = onlineCount;
		listState.groups = visibleGroups;
		listState.rows = prunedRows;
		listState.items = newItems;
		listState.presences = newPresences;
		listState.customStatuses = newCustomStatuses;
		listState.knownCustomStatuses = nextKnownCustomStatuses;
		listState.requestedRanges = requestedRanges;
		listState.subscribedRanges = getHydratedMemberListRanges(
			{
				memberCount: totalMembers,
				groups: visibleGroups,
				itemIndexes: newItems,
			},
			requestedRanges,
		);
		listState.hasReceivedInitialPayload = true;
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
		this.evictCachedListsForUsers(
			guildId,
			new Set([...changedPresenceUserIds, ...changedCustomStatusUserIds]),
			storageKey,
		);
	}

	private getMemberListUpdateDemand(params: MemberListUpdateParams): MemberListUpdateDemand | null {
		const active = this.activeMemberListSubscription;
		if (active == null) {
			return null;
		}
		const demand: MemberListUpdateDemand = {
			generation: this.memberListSubscriptionGeneration,
			guildId: active.guildId,
			channelId: active.channelId,
			ownerId: active.ownerId,
		};
		if (!this.isMemberListUpdateAccepted(demand, params)) {
			return null;
		}
		return demand;
	}

	private isMemberListUpdateDemandCurrent(demand: MemberListUpdateDemand): boolean {
		if (demand.generation !== this.memberListSubscriptionGeneration) {
			return false;
		}
		const active = this.activeMemberListSubscription;
		if (
			active == null ||
			active.guildId !== demand.guildId ||
			active.channelId !== demand.channelId ||
			active.ownerId !== demand.ownerId
		) {
			return false;
		}
		const storageKey = this.resolveListKey(demand.guildId, demand.channelId);
		const guildLists = this.lists[demand.guildId];
		const listState = guildLists == null ? undefined : guildLists[storageKey];
		return listState != null && listState.requestedRanges.length > 0;
	}

	private isMemberListUpdateAccepted(demand: MemberListUpdateDemand, params: MemberListUpdateParams): boolean {
		if (!this.isMemberListUpdateDemandCurrent(demand) || params.guildId !== demand.guildId) {
			return false;
		}
		if (params.channelId != null) {
			return params.channelId === demand.channelId;
		}
		const storageKey = this.resolveListKey(demand.guildId, demand.channelId);
		return params.listId === storageKey;
	}

	private areMemberListUpdateDemandsEqual(left: MemberListUpdateDemand, right: MemberListUpdateDemand): boolean {
		return (
			left.generation === right.generation &&
			left.guildId === right.guildId &&
			left.channelId === right.channelId &&
			left.ownerId === right.ownerId
		);
	}

	handleGatewayDisconnected(): void {
		this.listSubscribedChannelIds = {};
		this.syncedMemberListGuildIds.clear();
		this.clearSentMemberListSubscription();
	}

	private convertRow(rawItem: MemberListOperationItem): MemberListRow | null {
		if (rawItem.group) {
			if (Math.max(0, rawItem.group.count) === 0) {
				return null;
			}
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
			member: rawItem.member,
			presence: rawItem.member.presence ?? null,
		};
	}

	private visibleGroups(groups: ReadonlyArray<MemberListGroup>): Array<MemberListGroup> {
		return groups.filter((group) => Math.max(0, group.count) > 0);
	}

	private isEmptyGroupItem(rawItem: MemberListOperationItem): boolean {
		return rawItem.group !== undefined && Math.max(0, rawItem.group.count) === 0;
	}

	private convertItem(guildId: string, row: MemberListRow): MemberListItem | null {
		if (row.member && row.userId) {
			return {
				type: 'member',
				data: {
					userId: row.userId,
					member: row.member,
				},
			};
		}
		this.logger.warn('Member not found in store:', {guildId, userId: row.userId});
		return null;
	}

	materializeItemMember(guildId: string, item: MemberListItem | null | undefined): GuildMember | null {
		if (!item) {
			return null;
		}
		const cachedMember = this.materializedMemberCache.get(item.data);
		if (cachedMember) {
			return cachedMember;
		}
		const member = new GuildMember(guildId, item.data.member, {cacheUser: false});
		this.materializedMemberCache.set(item.data, member);
		return member;
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

	subscribeToChannel(
		guildId: string,
		channelId: string,
		ranges: Array<[number, number]>,
		ownerId: string | null = null,
	): boolean {
		if (this.isMemberListUpdatesDisabled(guildId)) {
			return false;
		}
		if (ownerId == null) {
			this.claimMemberListSubscription(guildId, channelId, null);
		} else if (!this.isActiveMemberListSubscriptionOwner(guildId, channelId, ownerId)) {
			return false;
		}
		return this.applyChannelSubscription(guildId, channelId, ranges);
	}

	retryChannelSubscription(
		guildId: string,
		channelId: string,
		ranges: Array<[number, number]>,
		ownerId: string,
	): boolean {
		if (!this.isActiveMemberListSubscriptionOwner(guildId, channelId, ownerId)) {
			return false;
		}
		return this.applyChannelSubscription(guildId, channelId, ranges);
	}

	updateChannelSubscriptionRangesLocally(
		guildId: string,
		channelId: string,
		ranges: MemberListRanges,
		ownerId: string,
	): boolean {
		if (!this.isActiveMemberListSubscriptionOwner(guildId, channelId, ownerId)) {
			return false;
		}
		this.applyLocalChannelSubscription(guildId, channelId, normalizeMemberListRanges(ranges));
		return true;
	}

	private applyChannelSubscription(guildId: string, channelId: string, ranges: Array<[number, number]>): boolean {
		const normalizedRanges = normalizeMemberListRanges(ranges);
		this.pruneMemberListCache();
		const storageKey = this.resolveListKey(guildId, channelId);
		const socket = GatewayConnection.socket;
		const guildSubscriptions = this.listSubscribedChannelIds[guildId];
		const currentSubscribedChannelId = guildSubscriptions == null ? undefined : guildSubscriptions[storageKey];
		const shouldSendUpdate =
			this.sentMemberListGuildId !== guildId ||
			this.sentMemberListChannelId !== channelId ||
			currentSubscribedChannelId !== channelId ||
			!areRangesEqual(this.sentMemberListRanges, normalizedRanges);
		const shouldBootstrapGuildSync = !this.syncedMemberListGuildIds.has(guildId);
		let delivered = !shouldSendUpdate;
		if (shouldSendUpdate) {
			if (socket != null && socket.isConnected() === true) {
				delivered = true;
				socket.updateGuildSubscriptions({
					subscriptions: {
						[guildId]: {
							active: true,
							...(shouldBootstrapGuildSync ? {sync: true} : {}),
							member_list_channels: {[channelId]: normalizedRanges},
						},
					},
				});
				this.setListSubscribedChannel(guildId, storageKey, channelId);
				this.sentMemberListGuildId = guildId;
				this.sentMemberListChannelId = channelId;
				this.sentMemberListRanges = normalizedRanges;
				this.syncedMemberListGuildIds.add(guildId);
			}
		}
		this.applyLocalChannelSubscription(guildId, channelId, normalizedRanges);
		return delivered;
	}

	private applyLocalChannelSubscription(
		guildId: string,
		channelId: string,
		normalizedRanges: NormalizedMemberListRanges,
	): void {
		this.pruneMemberListCache();
		const storageKey = this.resolveListKey(guildId, channelId);
		const existingGuildLists = this.lists[guildId] ?? {};
		const guildLists: Record<string, MemberListState> = {...existingGuildLists};
		const existingList = guildLists[storageKey];
		if (!existingList) {
			guildLists[storageKey] = this.createEmptyListState(normalizedRanges);
		} else {
			guildLists[storageKey] = this.pruneListStateToRanges({
				listState: existingList,
				requestedRanges: normalizedRanges,
			});
		}
		this.touchList(guildId, storageKey);
		this.lists = {...this.lists, [guildId]: guildLists};
		this.pruneMemberListCache();
	}

	unsubscribeFromChannel(
		guildId: string,
		channelId: string,
		clearLocalSubscription = true,
		ownerId?: string | null,
	): void {
		this.clearChannelSubscription({
			guildId,
			channelId,
			clearLocalSubscription,
			ownerId,
			updateGateway: true,
		});
	}

	releaseMemberListSubscription(guildId: string, channelId: string, ownerId: string): void {
		this.clearChannelSubscription({
			guildId,
			channelId,
			clearLocalSubscription: true,
			ownerId,
			updateGateway: false,
		});
	}

	private clearChannelSubscription({
		guildId,
		channelId,
		clearLocalSubscription,
		ownerId,
		updateGateway,
	}: {
		guildId: string;
		channelId: string;
		clearLocalSubscription: boolean;
		ownerId?: string | null;
		updateGateway: boolean;
	}): void {
		if (ownerId !== undefined) {
			if (!this.isActiveMemberListSubscriptionOwner(guildId, channelId, ownerId)) {
				return;
			}
			this.setActiveMemberListSubscription(null);
		} else if (
			this.activeMemberListSubscription?.guildId === guildId &&
			this.activeMemberListSubscription?.channelId === channelId
		) {
			this.setActiveMemberListSubscription(null);
		}
		const storageKey = this.resolveListKey(guildId, channelId);
		const currentSubscribedChannelId = this.listSubscribedChannelIds[guildId]?.[storageKey];
		if (currentSubscribedChannelId && currentSubscribedChannelId !== channelId) {
			return;
		}
		if (updateGateway) {
			const socket = GatewayConnection.socket;
			if (socket != null && socket.isConnected() === true) {
				socket.updateGuildSubscriptions({
					subscriptions: {
						[guildId]: {
							member_list_channels: {[channelId]: []},
						},
					},
				});
			}
		}
		this.clearListSubscribedChannel(guildId, storageKey);
		this.clearPendingListUpdateBatch(guildId, storageKey);
		const existingGuildLists = this.lists[guildId] ?? {};
		const existingList = existingGuildLists[storageKey];
		if (existingList && clearLocalSubscription) {
			const guildLists = {...existingGuildLists};
			guildLists[storageKey] = {
				...existingList,
				requestedRanges: EMPTY_MEMBER_LIST_RANGES,
				subscribedRanges: EMPTY_MEMBER_LIST_RANGES,
			};
			this.touchList(guildId, storageKey);
			this.lists = {...this.lists, [guildId]: guildLists};
			this.pruneMemberListCache();
		}
	}

	claimMemberListSubscription(guildId: string, channelId: string, ownerId: string | null): void {
		if (this.isMemberListUpdatesDisabled(guildId)) {
			return;
		}
		const activeSubscription = this.activeMemberListSubscription;
		if (
			activeSubscription != null &&
			(activeSubscription.guildId !== guildId || activeSubscription.channelId !== channelId)
		) {
			this.clearChannelSubscription({
				guildId: activeSubscription.guildId,
				channelId: activeSubscription.channelId,
				clearLocalSubscription: true,
				ownerId: activeSubscription.ownerId,
				updateGateway: false,
			});
		}
		this.unsubscribeKnownMemberListSubscriptionsExcept({guildId, channelId});
		this.setActiveMemberListSubscription({guildId, channelId, ownerId});
	}

	private setActiveMemberListSubscription(subscription: MemberListSubscription | null): void {
		const current = this.activeMemberListSubscription;
		if (current === subscription) {
			return;
		}
		if (
			current != null &&
			subscription != null &&
			current.guildId === subscription.guildId &&
			current.channelId === subscription.channelId &&
			current.ownerId === subscription.ownerId
		) {
			return;
		}
		this.activeMemberListSubscription = subscription;
		this.memberListSubscriptionGeneration += 1;
	}

	isActiveMemberListSubscriptionOwner(guildId: string, channelId: string, ownerId: string | null): boolean {
		const active = this.activeMemberListSubscription;
		return active != null && active.guildId === guildId && active.channelId === channelId && active.ownerId === ownerId;
	}

	hasActiveMemberListSubscription(): boolean {
		return this.activeMemberListSubscription != null;
	}

	getVisibleItems(guildId: string, listId: string, rowRange: [number, number]): Array<MemberListItem> {
		const listState = this.getList(guildId, listId);
		if (!listState) {
			return [];
		}
		const [start, end] = rowRange;
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
		const listState = this.lists[guildId]?.[storageKey];
		if (!listState || this.isInactiveListExpired(guildId, storageKey, Date.now(), listState)) {
			return undefined;
		}
		return listState;
	}

	getListIdentityKey(guildId: string, channelId: string): string {
		return this.getLocalChannelListKey(guildId, channelId) ?? this.resolveListKey(guildId, channelId);
	}

	hasHydratedRanges(guildId: string, channelId: string, ranges: MemberListRanges): boolean {
		const listState = this.getList(guildId, channelId);
		if (listState == null || !listState.hasReceivedInitialPayload) {
			return false;
		}
		return areNormalizedMemberListRangesCovered(normalizeMemberListRanges(ranges), listState.subscribedRanges);
	}

	getMemberCount(guildId: string, listId: string): number {
		return this.getList(guildId, listId)?.memberCount ?? 0;
	}

	getOnlineCount(guildId: string, listId: string): number {
		return this.getList(guildId, listId)?.onlineCount ?? 0;
	}

	getPresence(guildId: string, listId: string, userId: string): StatusType | null {
		const listState = this.getList(guildId, listId);
		if (!listState) {
			return null;
		}
		return listState.presences.get(userId) ?? null;
	}

	getCustomStatus(guildId: string, listId: string, userId: string): CustomStatus | null | undefined {
		const listState = this.getList(guildId, listId);
		if (!listState) {
			return undefined;
		}
		if (listState.customStatuses.has(userId)) {
			return listState.customStatuses.get(userId) ?? null;
		}
		if (listState.knownCustomStatuses.has(userId)) {
			return listState.knownCustomStatuses.get(userId) ?? null;
		}
		return undefined;
	}

	handleLocalPresenceUpdate(userId: string, status: StatusType, customStatus: CustomStatus | null): void {
		const changedCustomStatusListIds: Array<{guildId: string; listId: string}> = [];
		let didUpdate = false;
		const updatedLists: Record<string, Record<string, MemberListState>> = {...this.lists};
		for (const [guildId, guildLists] of Object.entries(this.lists)) {
			let nextGuildLists: Record<string, MemberListState> | null = null;
			for (const [listId, listState] of Object.entries(guildLists)) {
				if (!this.listStateContainsUser(listState, userId)) {
					continue;
				}
				const currentStatus = listState.presences.get(userId) ?? null;
				const currentCustomStatus = listState.customStatuses.has(userId)
					? (listState.customStatuses.get(userId) ?? null)
					: (listState.knownCustomStatuses.get(userId) ?? null);
				if (currentStatus === status && areCustomStatusesEqual(currentCustomStatus, customStatus)) {
					continue;
				}
				const nextPresences = new Map(listState.presences);
				nextPresences.set(userId, status);
				const nextCustomStatuses = new Map(listState.customStatuses);
				nextCustomStatuses.set(userId, customStatus);
				const nextKnownCustomStatuses = new Map(listState.knownCustomStatuses);
				nextKnownCustomStatuses.set(userId, customStatus);
				if (!nextGuildLists) {
					nextGuildLists = {...guildLists};
				}
				nextGuildLists[listId] = {
					...listState,
					presences: nextPresences,
					customStatuses: nextCustomStatuses,
					knownCustomStatuses: nextKnownCustomStatuses,
				};
				changedCustomStatusListIds.push({guildId, listId});
				didUpdate = true;
			}
			if (nextGuildLists) {
				updatedLists[guildId] = nextGuildLists;
			}
		}
		if (!didUpdate) {
			return;
		}
		this.lists = updatedLists;
		queueMicrotask(() => {
			for (const {guildId, listId} of changedCustomStatusListIds) {
				CustomStatusEmitter.emitMemberListChange(guildId, listId, userId);
			}
		});
	}

	private pruneListStateToRanges(params: {
		listState: MemberListState;
		requestedRanges: MemberListRanges;
	}): MemberListState {
		const {listState, requestedRanges} = params;
		const normalizedRequestedRanges = normalizeMemberListRanges(requestedRanges);
		if (normalizedRequestedRanges.length === 0) {
			return {
				...listState,
				requestedRanges: EMPTY_MEMBER_LIST_RANGES,
				subscribedRanges: EMPTY_MEMBER_LIST_RANGES,
				rows: new Map(),
				items: new Map(),
				presences: new Map(),
				customStatuses: new Map(),
			};
		}
		const prunedRows = this.pruneRowsToRanges(listState.rows, normalizedRequestedRanges);
		const prunedItems = this.pruneItemsToRanges(listState.items, normalizedRequestedRanges);
		const retainedUserIds = new Set<string>();
		for (const item of prunedItems.values()) {
			retainedUserIds.add(item.data.userId);
		}
		const prunedPresences = new Map<string, StatusType>();
		const prunedCustomStatuses = new Map<string, CustomStatus | null>();
		for (const [userId, status] of listState.presences) {
			if (retainedUserIds.has(userId)) {
				prunedPresences.set(userId, status);
			}
		}
		for (const [userId, customStatus] of listState.customStatuses) {
			if (retainedUserIds.has(userId)) {
				prunedCustomStatuses.set(userId, customStatus);
			}
		}
		return {
			...listState,
			requestedRanges: normalizedRequestedRanges,
			subscribedRanges: listState.hasReceivedInitialPayload
				? getHydratedMemberListRanges(
						{
							memberCount: listState.memberCount,
							groups: listState.groups,
							itemIndexes: prunedItems,
						},
						normalizedRequestedRanges,
					)
				: EMPTY_MEMBER_LIST_RANGES,
			rows: prunedRows,
			items: prunedItems,
			presences: prunedPresences,
			customStatuses: prunedCustomStatuses,
		};
	}

	private pruneRowsToRanges(
		rows: Map<number, MemberListRow>,
		subscribedRanges: Array<[number, number]>,
	): Map<number, MemberListRow> {
		if (subscribedRanges.length === 0) {
			return new Map();
		}
		const prunedRows = new Map<number, MemberListRow>();
		for (const [index, row] of rows) {
			if (isIndexInMemberListRanges(index, subscribedRanges)) {
				prunedRows.set(index, row);
			}
		}
		return prunedRows;
	}

	private visitRowsInIndexOrder(
		rows: Map<number, MemberListRow>,
		subscribedRanges: NormalizedMemberListRanges,
		visit: (rowIndex: number, row: MemberListRow) => void,
	): void {
		if (subscribedRanges.length === 0) {
			const sortedRows = Array.from(rows.entries()).sort(([left], [right]) => left - right);
			for (const [rowIndex, row] of sortedRows) {
				visit(rowIndex, row);
			}
			return;
		}
		for (const [start, end] of subscribedRanges) {
			for (let rowIndex = start; rowIndex <= end; rowIndex += 1) {
				const row = rows.get(rowIndex);
				if (row) {
					visit(rowIndex, row);
				}
			}
		}
	}

	private pruneItemsToRanges(
		items: Map<number, MemberListItem>,
		subscribedRanges: Array<[number, number]>,
	): Map<number, MemberListItem> {
		if (subscribedRanges.length === 0) {
			return new Map();
		}
		const prunedItems = new Map<number, MemberListItem>();
		for (const [rowIndex, item] of items) {
			if (isIndexInMemberListRanges(rowIndex, subscribedRanges)) {
				prunedItems.set(rowIndex, item);
			}
		}
		return prunedItems;
	}

	private createEmptyListState(requestedRanges: MemberListRanges = []): MemberListState {
		return {
			hasReceivedInitialPayload: false,
			memberCount: 0,
			onlineCount: 0,
			groups: [],
			rows: new Map(),
			items: new Map(),
			requestedRanges: normalizeMemberListRanges(requestedRanges),
			subscribedRanges: EMPTY_MEMBER_LIST_RANGES,
			presences: new Map(),
			customStatuses: new Map(),
			knownCustomStatuses: new Map(),
		};
	}

	private isMemberListUpdatesDisabled(guildId: string): boolean {
		const guild = Guilds.getGuild(guildId);
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
		const mappedListId = guildMappings?.[listIdOrChannelId];
		if (mappedListId) {
			return mappedListId;
		}
		const localStorageKey = this.getLocalChannelListKey(guildId, listIdOrChannelId);
		if (!localStorageKey) {
			return listIdOrChannelId;
		}
		return this.permissionListIds[guildId]?.[localStorageKey] ?? localStorageKey;
	}

	private registerChannelListId(guildId: string, channelId: string, listId: string): void {
		const guildMappings = this.channelListIds[guildId] ?? {};
		if (guildMappings[channelId] !== listId) {
			this.channelListIds = {
				...this.channelListIds,
				[guildId]: {...guildMappings, [channelId]: listId},
			};
		} else if (!this.channelListIds[guildId]) {
			this.channelListIds = {...this.channelListIds, [guildId]: guildMappings};
		}
		const localStorageKey = this.getLocalChannelListKey(guildId, channelId);
		if (!localStorageKey) {
			return;
		}
		const guildPermissionMappings = this.permissionListIds[guildId] ?? {};
		if (guildPermissionMappings[localStorageKey] === listId) {
			if (!this.permissionListIds[guildId]) {
				this.permissionListIds = {...this.permissionListIds, [guildId]: guildPermissionMappings};
			}
			return;
		}
		this.permissionListIds = {
			...this.permissionListIds,
			[guildId]: {...guildPermissionMappings, [localStorageKey]: listId},
		};
	}

	private getLocalChannelListKey(guildId: string, channelId: string): string | undefined {
		const channel = Channels.getChannel(channelId);
		if (!channel || channel.guildId !== guildId || channel.isPrivate()) {
			return undefined;
		}
		const overwrites = Object.values(channel.permissionOverwrites)
			.map((overwrite) => {
				const allowViewChannel = (overwrite.allow & Permissions.VIEW_CHANNEL) === Permissions.VIEW_CHANNEL;
				const denyViewChannel =
					!allowViewChannel && (overwrite.deny & Permissions.VIEW_CHANNEL) === Permissions.VIEW_CHANNEL;
				if (!allowViewChannel && !denyViewChannel) {
					return null;
				}
				return {
					id: overwrite.id,
					type: overwrite.type,
					view: allowViewChannel ? 'allow' : 'deny',
				};
			})
			.filter((overwrite): overwrite is {id: string; type: number; view: 'allow' | 'deny'} => overwrite != null)
			.sort((left, right) => left.id.localeCompare(right.id) || left.type - right.type)
			.map((overwrite) => [overwrite.id, overwrite.type.toString(), overwrite.view].join(':'))
			.join(',');
		return `permission:${guildId}:${overwrites}`;
	}

	private moveListAccess(guildId: string, fromListId: string, toListId: string): void {
		if (fromListId === toListId) {
			return;
		}
		const guildAccess = this.lastAccess[guildId];
		if (!guildAccess || guildAccess[fromListId] == null) {
			return;
		}
		const nextGuildAccess = {...guildAccess};
		nextGuildAccess[toListId] = Math.max(nextGuildAccess[toListId] ?? 0, nextGuildAccess[fromListId]);
		delete nextGuildAccess[fromListId];
		this.lastAccess = {...this.lastAccess, [guildId]: nextGuildAccess};
	}

	private setListSubscribedChannel(guildId: string, listId: string, channelId: string): void {
		const guildSubscriptions = this.listSubscribedChannelIds[guildId] ?? {};
		if (guildSubscriptions[listId] === channelId) {
			if (!this.listSubscribedChannelIds[guildId]) {
				this.listSubscribedChannelIds = {...this.listSubscribedChannelIds, [guildId]: guildSubscriptions};
			}
			return;
		}
		this.listSubscribedChannelIds = {
			...this.listSubscribedChannelIds,
			[guildId]: {...guildSubscriptions, [listId]: channelId},
		};
	}

	private clearSentMemberListSubscription(): void {
		this.sentMemberListGuildId = null;
		this.sentMemberListChannelId = null;
		this.sentMemberListRanges = EMPTY_MEMBER_LIST_RANGES;
	}

	private clearListSubscribedChannel(guildId: string, listId: string): void {
		const guildSubscriptions = this.listSubscribedChannelIds[guildId];
		if (guildSubscriptions != null && guildSubscriptions[listId] != null) {
			const {[listId]: _, ...remainingGuildSubscriptions} = guildSubscriptions;
			if (Object.keys(remainingGuildSubscriptions).length === 0) {
				const {[guildId]: __, ...remainingSubscriptions} = this.listSubscribedChannelIds;
				this.listSubscribedChannelIds = remainingSubscriptions;
			} else {
				this.listSubscribedChannelIds = {...this.listSubscribedChannelIds, [guildId]: remainingGuildSubscriptions};
			}
		}
	}

	private moveListSubscribedChannel(guildId: string, fromListId: string, toListId: string): void {
		if (fromListId === toListId) {
			return;
		}
		const guildSubscriptions = this.listSubscribedChannelIds[guildId];
		if (guildSubscriptions != null && guildSubscriptions[fromListId] != null) {
			const nextGuildSubscriptions = {...guildSubscriptions};
			nextGuildSubscriptions[toListId] = nextGuildSubscriptions[fromListId];
			delete nextGuildSubscriptions[fromListId];
			this.listSubscribedChannelIds = {...this.listSubscribedChannelIds, [guildId]: nextGuildSubscriptions};
		}
	}

	private unsubscribeKnownMemberListSubscriptionsExcept(target: {guildId: string; channelId: string} | null): void {
		const subscriptions = new Map<string, {guildId: string; channelId: string}>();
		for (const [guildId, guildSubscriptions] of Object.entries(this.listSubscribedChannelIds)) {
			for (const channelId of Object.values(guildSubscriptions)) {
				if (target && target.guildId === guildId && target.channelId === channelId) {
					continue;
				}
				subscriptions.set(`${guildId}:${channelId}`, {guildId, channelId});
			}
		}
		for (const subscription of subscriptions.values()) {
			this.clearChannelSubscription({
				guildId: subscription.guildId,
				channelId: subscription.channelId,
				clearLocalSubscription: true,
				updateGateway: false,
			});
		}
	}

	private isInactiveListExpired(guildId: string, listId: string, now: number, listState: MemberListState): boolean {
		if (listState.requestedRanges.length > 0) {
			return false;
		}
		const lastSeen = this.lastAccess[guildId]?.[listId] ?? 0;
		return lastSeen > 0 && now - lastSeen > MEMBER_LIST_TTL_MS;
	}

	private pruneMemberListCache(now = Date.now()): void {
		for (const [guildId, guildLists] of Object.entries(this.lists)) {
			for (const [listId, listState] of Object.entries(guildLists)) {
				if (this.isInactiveListExpired(guildId, listId, now, listState)) {
					this.evictList(guildId, listId);
				}
			}
		}
		const inactiveLists: Array<{guildId: string; listId: string; lastSeen: number}> = [];
		for (const [guildId, guildLists] of Object.entries(this.lists)) {
			for (const [listId, listState] of Object.entries(guildLists)) {
				if (listState.requestedRanges.length > 0) {
					continue;
				}
				inactiveLists.push({
					guildId,
					listId,
					lastSeen: this.lastAccess[guildId]?.[listId] ?? 0,
				});
			}
		}
		if (inactiveLists.length <= MEMBER_LIST_CACHE_MAX_LISTS) {
			return;
		}
		inactiveLists.sort((left, right) => left.lastSeen - right.lastSeen);
		for (let index = 0; index < inactiveLists.length - MEMBER_LIST_CACHE_MAX_LISTS; index += 1) {
			const {guildId, listId} = inactiveLists[index];
			this.evictList(guildId, listId);
		}
	}

	private evictCachedListsForUsers(guildId: string, userIds: Set<string>, sourceListId: string): void {
		if (userIds.size === 0) {
			return;
		}
		const guildLists = this.lists[guildId];
		if (!guildLists) {
			return;
		}
		for (const [listId, listState] of Object.entries(guildLists)) {
			if (listId === sourceListId || listState.requestedRanges.length > 0) {
				continue;
			}
			for (const userId of userIds) {
				if (this.listStateContainsUser(listState, userId)) {
					this.evictList(guildId, listId);
					break;
				}
			}
		}
	}

	private listStateContainsUser(listState: MemberListState, userId: string): boolean {
		for (const item of listState.items.values()) {
			if (item.data.userId === userId) {
				return true;
			}
		}
		for (const row of listState.rows.values()) {
			if (row.userId === userId) {
				return true;
			}
		}
		return (
			listState.presences.has(userId) ||
			listState.customStatuses.has(userId) ||
			listState.knownCustomStatuses.has(userId)
		);
	}

	private evictList(guildId: string, listId: string): void {
		this.clearPendingListUpdateBatch(guildId, listId);
		const existingGuildLists = this.lists[guildId] ?? {};
		if (existingGuildLists[listId]) {
			const {[listId]: _, ...remainingGuildLists} = existingGuildLists;
			if (Object.keys(remainingGuildLists).length === 0) {
				const {[guildId]: __, ...remainingLists} = this.lists;
				this.lists = remainingLists;
			} else {
				this.lists = {...this.lists, [guildId]: remainingGuildLists};
			}
		}
		const existingAccess = this.lastAccess[guildId] ?? {};
		if (existingAccess[listId] != null) {
			const {[listId]: _, ...remainingAccess} = existingAccess;
			if (Object.keys(remainingAccess).length === 0) {
				const {[guildId]: __, ...allAccess} = this.lastAccess;
				this.lastAccess = allAccess;
			} else {
				this.lastAccess = {...this.lastAccess, [guildId]: remainingAccess};
			}
		}
		this.clearListSubscribedChannel(guildId, listId);
		this.removeListAliases(guildId, listId);
	}

	private removeListAliases(guildId: string, listId: string): void {
		const guildMappings = this.channelListIds[guildId];
		if (guildMappings) {
			const nextGuildMappings: Record<string, string> = {};
			for (const [channelId, mappedListId] of Object.entries(guildMappings)) {
				if (mappedListId !== listId && channelId !== listId) {
					nextGuildMappings[channelId] = mappedListId;
				}
			}
			if (Object.keys(nextGuildMappings).length === 0) {
				const {[guildId]: _, ...remainingMappings} = this.channelListIds;
				this.channelListIds = remainingMappings;
			} else {
				this.channelListIds = {...this.channelListIds, [guildId]: nextGuildMappings};
			}
		}
		const permissionMappings = this.permissionListIds[guildId];
		if (!permissionMappings) {
			return;
		}
		const nextPermissionMappings: Record<string, string> = {};
		for (const [permissionKey, mappedListId] of Object.entries(permissionMappings)) {
			if (mappedListId !== listId && permissionKey !== listId) {
				nextPermissionMappings[permissionKey] = mappedListId;
			}
		}
		if (Object.keys(nextPermissionMappings).length === 0) {
			const {[guildId]: _, ...remainingPermissionMappings} = this.permissionListIds;
			this.permissionListIds = remainingPermissionMappings;
		} else {
			this.permissionListIds = {...this.permissionListIds, [guildId]: nextPermissionMappings};
		}
	}

	private startPruneInterval(): void {
		if (this.pruneIntervalId != null) {
			return;
		}
		if (typeof window === 'undefined') {
			return;
		}
		this.pruneIntervalId = window.setInterval(() => this.pruneExpiredLists(), MEMBER_LIST_PRUNE_INTERVAL_MS);
	}

	private pruneExpiredLists(): void {
		if (!Window.focused) {
			return;
		}
		this.pruneMemberListCache();
	}
}

export default new MemberSidebar();
