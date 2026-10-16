// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReadyPayload} from '@app/features/gateway/events/GatewayReady';
import type {SnapshotUserUpdatePayload} from '@app/features/gateway/snapshot/SnapshotDispatchEvent';
import type {
	SnapshotEmit,
	SnapshotPresenceRow,
	SnapshotRowReplaceEntityEntry,
	SnapshotUserRow,
} from '@app/features/gateway/snapshot/SnapshotEntities';
import {
	parseSnapshotEntity,
	snapshotPresenceKey,
	snapshotUserKey,
} from '@app/features/gateway/snapshot/SnapshotEntities';
import type {ReducerRowMap} from '@app/features/gateway/snapshot/SnapshotReducerBacking';
import type {StateSnapshotEntries} from '@app/features/gateway/snapshot/SnapshotTypes';
import type {PresenceRecord} from '@app/features/gateway/types/GatewayPresenceTypes';
import type {RelationshipWire} from '@app/features/relationship/models/Relationship';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';

export interface SnapshotUserShellState {
	readonly selfUserId: string | null;
	readonly shellUserIds: ReadonlySet<string>;
}

function readyPresenceRow(presence: PresenceRecord, guildIds: ReadonlyArray<string>): SnapshotPresenceRow {
	return {
		status: presence.status ?? null,
		customStatus: presence.custom_status ?? null,
		afk: presence.afk ?? false,
		mobile: presence.mobile ?? false,
		guildIds: [...guildIds],
	};
}

function updatedPresenceRow(
	presence: PresenceRecord,
	prior: SnapshotPresenceRow | undefined,
	guildIds: ReadonlyArray<string>,
): SnapshotPresenceRow | null {
	const status = presence.status === undefined ? prior?.status : presence.status;
	const customStatus = presence.custom_status === undefined ? prior?.customStatus : presence.custom_status;
	const afk = presence.afk === undefined ? prior?.afk : presence.afk;
	const mobile = presence.mobile === undefined ? prior?.mobile : presence.mobile;
	if (status === undefined || customStatus === undefined || afk === undefined || mobile === undefined) {
		return null;
	}
	return {status, customStatus, afk, mobile, guildIds: [...guildIds]};
}

export class SnapshotUserReducer {
	private shellUserIds = new Set<string>();
	private selfUserId: string | null = null;

	constructor(
		private readonly users: ReducerRowMap<string, SnapshotUserRow>,
		private readonly presences: ReducerRowMap<string, SnapshotPresenceRow>,
	) {}

	get selfId(): string | null {
		return this.selfUserId;
	}

	get rows(): ReadonlyMap<string, SnapshotUserRow> {
		return this.users;
	}

	exportState(): SnapshotUserShellState {
		return {
			selfUserId: this.selfUserId,
			shellUserIds: new Set(this.shellUserIds),
		};
	}

	importState(state: SnapshotUserShellState): void {
		this.selfUserId = state.selfUserId;
		this.shellUserIds = new Set(state.shellUserIds);
	}

	load(entries: StateSnapshotEntries): void {
		this.users.clear();
		for (const [key, value] of parseSnapshotEntity(entries, 'user')) {
			this.users.set(key, value);
		}
		this.presences.clear();
		for (const [key, value] of parseSnapshotEntity(entries, 'presence')) {
			this.presences.set(key, value);
		}
	}

	initializeReady(data: ReadyPayload): void {
		this.users.clear();
		this.presences.clear();
		this.selfUserId = data.user.id;
		this.shellUserIds = this.computeShellUserIds(data);
	}

	absorbReadyMember(user: SnapshotUserRow): void {
		this.shellUserIds.add(user.id);
		this.users.set(user.id, this.mergeUserRow(this.users.get(user.id), user));
	}

	emitReady(
		emit: SnapshotEmit,
		data: ReadyPayload,
		memberGuildIdsByUser: ReadonlyMap<string, ReadonlySet<string>>,
	): void {
		const userEntries: Array<SnapshotRowReplaceEntityEntry<'user'>> = [];
		this.users.set(data.user.id, data.user);
		userEntries.push({key: snapshotUserKey(data.user.id), value: data.user});
		for (const user of data.users ?? []) {
			if (user.id === data.user.id || !this.shellUserIds.has(user.id)) {
				continue;
			}
			this.users.set(user.id, user);
			userEntries.push({key: snapshotUserKey(user.id), value: user});
		}
		emit({kind: 'replaceEntity', entity: 'user', entries: userEntries});

		const presenceGuildIdsByUser = new Map<string, Set<string>>();
		for (const [userId, guildIds] of memberGuildIdsByUser) {
			presenceGuildIdsByUser.set(userId, new Set(guildIds));
		}
		const meContextUserIds = this.computeReadyMeContextUserIds(data);
		const readyPresences = new Map<string, SnapshotPresenceRow>();
		for (const presence of data.presences ?? []) {
			const userId = presence.user.id;
			let guildIds = presenceGuildIdsByUser.get(userId);
			if (guildIds == null) {
				guildIds = new Set<string>();
				presenceGuildIdsByUser.set(userId, guildIds);
			}
			if (presence.guild_id) {
				guildIds.add(presence.guild_id);
			}
			if (meContextUserIds.has(userId) || guildIds.size === 0) {
				guildIds.add(ME);
			}
			if (!this.shellUserIds.has(userId)) {
				continue;
			}
			const row = readyPresenceRow(presence, Array.from(guildIds));
			readyPresences.set(userId, row);
		}
		const presenceEntries: Array<SnapshotRowReplaceEntityEntry<'presence'>> = [];
		for (const [userId, row] of readyPresences) {
			this.presences.set(userId, row);
			presenceEntries.push({key: snapshotPresenceKey(userId), value: row});
		}
		emit({kind: 'replaceEntity', entity: 'presence', entries: presenceEntries});
	}

	retainUser(emit: SnapshotEmit, user: SnapshotUserRow): void {
		this.shellUserIds.add(user.id);
		this.mergeAndEmit(emit, user);
	}

	applyGuildMember(emit: SnapshotEmit, user: SnapshotUserRow, guildId: string): void {
		this.retainUser(emit, user);
		const presence = this.presences.get(user.id);
		if (presence == null || presence.guildIds.includes(guildId)) {
			return;
		}
		const row: SnapshotPresenceRow = {...presence, guildIds: [...presence.guildIds, guildId]};
		this.presences.set(user.id, row);
		emit({kind: 'upsert', entity: 'presence', key: snapshotPresenceKey(user.id), value: row});
	}

	removeGuildMember(emit: SnapshotEmit, user: SnapshotUserRow, guildId: string): void {
		this.retainUser(emit, user);
		this.removePresenceGuildContext(emit, user.id, guildId);
	}

	applyRelationship(emit: SnapshotEmit, relationship: RelationshipWire): void {
		this.shellUserIds.add(relationship.id);
		if (relationship.user == null) {
			return;
		}
		this.shellUserIds.add(relationship.user.id);
		this.mergeAndEmit(emit, relationship.user);
	}

	applyUserUpdate(emit: SnapshotEmit, user: SnapshotUserUpdatePayload): void {
		if (!this.shellUserIds.has(user.id)) {
			return;
		}
		const existing = this.users.get(user.id);
		if (existing == null) {
			return;
		}
		const row: SnapshotUserRow = {...existing, ...user};
		this.users.set(user.id, row);
		emit({kind: 'upsert', entity: 'user', key: snapshotUserKey(user.id), value: row});
	}

	applyPresenceUpdate(emit: SnapshotEmit, presence: PresenceRecord): void {
		const userId = presence.user.id;
		if (!this.shellUserIds.has(userId)) {
			return;
		}
		this.mergeAndEmit(emit, presence.user);
		const prior = this.presences.get(userId);
		const guildIds = new Set(prior?.guildIds);
		if (presence.guild_id) {
			guildIds.add(presence.guild_id);
		} else {
			guildIds.add(ME);
		}
		const row = updatedPresenceRow(presence, prior, Array.from(guildIds));
		if (row === null) {
			return;
		}
		this.presences.set(userId, row);
		emit({kind: 'upsert', entity: 'presence', key: snapshotPresenceKey(userId), value: row});
	}

	applyGuildDelete(emit: SnapshotEmit, guildId: string): void {
		for (const [userId, presence] of this.presences) {
			if (!presence.guildIds.includes(guildId)) {
				continue;
			}
			const guildIds = presence.guildIds.filter((contextGuildId) => contextGuildId !== guildId);
			if (guildIds.length === 0) {
				this.presences.delete(userId);
				emit({kind: 'delete', entity: 'presence', key: snapshotPresenceKey(userId)});
				continue;
			}
			const row: SnapshotPresenceRow = {...presence, guildIds};
			this.presences.set(userId, row);
			emit({kind: 'upsert', entity: 'presence', key: snapshotPresenceKey(userId), value: row});
		}
	}

	private removePresenceGuildContext(emit: SnapshotEmit, userId: string, guildId: string): void {
		const presence = this.presences.get(userId);
		if (presence == null || !presence.guildIds.includes(guildId)) {
			return;
		}
		const guildIds = presence.guildIds.filter((contextGuildId) => contextGuildId !== guildId);
		if (guildIds.length === 0) {
			this.presences.delete(userId);
			emit({kind: 'delete', entity: 'presence', key: snapshotPresenceKey(userId)});
			return;
		}
		const row: SnapshotPresenceRow = {...presence, guildIds};
		this.presences.set(userId, row);
		emit({kind: 'upsert', entity: 'presence', key: snapshotPresenceKey(userId), value: row});
	}

	private computeShellUserIds(data: ReadyPayload): Set<string> {
		const ids = new Set<string>();
		ids.add(data.user.id);
		for (const relationship of data.relationships ?? []) {
			ids.add(relationship.id);
		}
		for (const channel of data.private_channels ?? []) {
			for (const recipient of channel.recipients ?? []) {
				ids.add(recipient.id);
			}
		}
		for (const guild of data.guilds) {
			if (guild.unavailable) {
				continue;
			}
			const ownerId = guild.properties.owner_id;
			if (ownerId) {
				ids.add(ownerId);
			}
		}
		return ids;
	}

	private computeReadyMeContextUserIds(data: ReadyPayload): Set<string> {
		const ids = new Set<string>();
		for (const relationship of data.relationships ?? []) {
			if (relationship.type === RelationshipTypes.FRIEND || relationship.type === RelationshipTypes.INCOMING_REQUEST) {
				ids.add(relationship.id);
			}
		}
		for (const channel of data.private_channels ?? []) {
			if (channel.type !== ChannelTypes.GROUP_DM) {
				continue;
			}
			for (const recipient of channel.recipients ?? []) {
				if (recipient.id !== data.user.id) {
					ids.add(recipient.id);
				}
			}
		}
		return ids;
	}

	private mergeAndEmit(emit: SnapshotEmit, update: SnapshotUserRow): void {
		const row = this.mergeUserRow(this.users.get(update.id), update);
		this.users.set(update.id, row);
		emit({kind: 'upsert', entity: 'user', key: snapshotUserKey(update.id), value: row});
	}

	private mergeUserRow(existing: SnapshotUserRow | undefined, update: SnapshotUserRow): SnapshotUserRow {
		return existing ? ({...existing, ...update} as SnapshotUserRow) : update;
	}
}
