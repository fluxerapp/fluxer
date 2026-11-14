// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReadyPayload} from '@app/features/gateway/events/GatewayReady';
import {SnapshotAccountReducer} from '@app/features/gateway/snapshot/SnapshotAccountReducer';
import {SnapshotChannelReducer} from '@app/features/gateway/snapshot/SnapshotChannelReducer';
import type {
	SnapshotDispatch,
	SnapshotGuildCountsUpdatePayload,
	SnapshotGuildDeletePayload,
	SnapshotGuildMemberListUpdatePayload,
	SnapshotGuildMemberPayload,
	SnapshotGuildMemberRemovePayload,
	SnapshotPassiveUpdatesPayload,
	SnapshotVoiceStateUpdatePayload,
} from '@app/features/gateway/snapshot/SnapshotDispatchEvent';
import type {
	SnapshotEmit,
	SnapshotEntity,
	SnapshotEntityKeyMap,
	SnapshotEntityRowMap,
	SnapshotGuildMemberKey,
	SnapshotGuildRow,
	SnapshotRowKeyPrefix,
	SnapshotRowOp,
	SnapshotRowReplaceEntityEntry,
	SnapshotUnavailableGuildRow,
} from '@app/features/gateway/snapshot/SnapshotEntities';
import {
	parseSnapshotEntity,
	SNAPSHOT_SCHEMA_EPOCH,
	snapshotGuildEmojiKey,
	snapshotGuildKey,
	snapshotGuildMemberKey,
	snapshotGuildRoleKey,
	snapshotGuildScopedPrefix,
	snapshotGuildStickerKey,
	snapshotRelationshipKey,
	snapshotUserGuildSettingsKey,
} from '@app/features/gateway/snapshot/SnapshotEntities';
import {
	createResidentSnapshotReducerBacking,
	type ReducerRowMap,
	type SnapshotReducerBacking,
} from '@app/features/gateway/snapshot/SnapshotReducerBacking';
import type {SnapshotReducerShellState} from '@app/features/gateway/snapshot/SnapshotReducerShellState';
import type {StateSnapshotEntries, StateSyncCursor} from '@app/features/gateway/snapshot/SnapshotTypes';
import {SnapshotUserReducer} from '@app/features/gateway/snapshot/SnapshotUserReducer';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import type {PresenceRecord} from '@app/features/gateway/types/GatewayPresenceTypes';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import type {RelationshipWire} from '@app/features/relationship/models/Relationship';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

function unsupportedSnapshotDispatch(dispatch: never): never {
	throw new Error(`Unsupported snapshot dispatch ${String(dispatch)}`);
}

function guildRowFromReadyData(data: GuildReadyData): SnapshotGuildRow {
	return {
		id: data.id,
		properties: data.properties,
		joined_at: data.joined_at ?? null,
		member_count: data.member_count,
		online_count: data.online_count,
		...(data.threads !== undefined ? {threads_active: true} : {}),
	};
}

function guildRowFromWireGuild(data: WireGuild, prior: SnapshotGuildRow | undefined): SnapshotGuildRow | null {
	const memberCount = data.member_count ?? prior?.member_count;
	if (memberCount === undefined) {
		return null;
	}
	const {roles: _roles, ...properties} = data;
	return {
		id: data.id,
		properties,
		joined_at: data.joined_at ?? prior?.joined_at ?? null,
		member_count: memberCount,
		online_count: data.online_count ?? prior?.online_count,
		...(prior?.threads_active === true ? {threads_active: true} : {}),
	};
}

function unavailableGuildRow(guildId: string): SnapshotUnavailableGuildRow {
	return {
		id: guildId,
		unavailable: true,
	};
}

function isUnavailableGuildCreate(data: GuildReadyData): boolean {
	return data.unavailable === true;
}

function isStaleSessionVoiceState(existing: VoiceState, update: SnapshotVoiceStateUpdatePayload): boolean {
	return (
		update.channel_id != null &&
		update.session_id != null &&
		existing.user_id === update.user_id &&
		existing.session_id === update.session_id &&
		existing.channel_id === update.channel_id
	);
}

export class SnapshotReducer {
	private readonly guilds: ReducerRowMap<string, SnapshotGuildRow>;
	private readonly unavailableGuilds: ReducerRowMap<string, SnapshotUnavailableGuildRow>;
	private readonly guildMembers: ReducerRowMap<SnapshotGuildMemberKey, GuildMemberData>;
	private readonly voiceStates: ReducerRowMap<string, ReadonlyArray<VoiceState>>;
	private readonly channelReducer: SnapshotChannelReducer;
	private readonly userReducer: SnapshotUserReducer;

	private readonly accountReducer = new SnapshotAccountReducer();

	constructor(backing?: SnapshotReducerBacking) {
		const rows = backing ?? createResidentSnapshotReducerBacking();
		this.guilds = rows.guilds;
		this.unavailableGuilds = rows.unavailableGuilds;
		this.guildMembers = rows.guildMembers;
		this.voiceStates = rows.voiceStates;
		this.userReducer = new SnapshotUserReducer(rows.users, rows.presences);
		this.channelReducer = new SnapshotChannelReducer(rows.channels, rows.readStates, {
			getSelfUserId: () => this.userReducer.selfId,
			retainUser: (emit, user) => this.userReducer.retainUser(emit, user),
		});
	}

	exportShellState(): SnapshotReducerShellState {
		const account = this.accountReducer.exportState();
		const user = this.userReducer.exportState();
		return {
			selfUserId: user.selfUserId,
			shellUserIds: user.shellUserIds,
			userSettings: account.userSettings,
			accountMetadata: account.accountMetadata,
		};
	}

	importShellState(state: SnapshotReducerShellState): void {
		this.userReducer.importState(state);
		this.accountReducer.importState(state);
	}

	loadHeavyMapsFromEntries(entries: StateSnapshotEntries): void {
		this.guilds.clear();
		for (const [key, value] of parseSnapshotEntity(entries, 'guild')) {
			this.guilds.set(key, value);
		}
		this.unavailableGuilds.clear();
		for (const [key, value] of parseSnapshotEntity(entries, 'unavailable_guild')) {
			this.unavailableGuilds.set(key, value);
		}
		this.guildMembers.clear();
		for (const [key, value] of parseSnapshotEntity(entries, 'guild_member')) {
			this.guildMembers.set(key, value);
		}
		this.voiceStates.clear();
		for (const [key, value] of parseSnapshotEntity(entries, 'voice_state')) {
			this.voiceStates.set(key, value);
		}
		this.channelReducer.load(entries);
		this.userReducer.load(entries);
		this.accountReducer.load(entries);
	}

	applyReady(emit: SnapshotEmit, data: ReadyPayload): StateSyncCursor {
		this.guilds.clear();
		this.unavailableGuilds.clear();
		this.guildMembers.clear();
		this.voiceStates.clear();
		this.channelReducer.reset();
		this.accountReducer.initializeReady(data);
		this.userReducer.initializeReady(data);

		const availableGuilds = data.guilds.filter((guild) => !guild.unavailable);
		const unavailableGuildEntries: Array<SnapshotRowReplaceEntityEntry<'unavailable_guild'>> = [];
		for (const guild of data.guilds) {
			if (guild.unavailable !== true) {
				continue;
			}
			const row = unavailableGuildRow(guild.id);
			this.unavailableGuilds.set(guild.id, row);
			unavailableGuildEntries.push({key: snapshotGuildKey(guild.id), value: row});
		}

		const guildEntries: Array<SnapshotRowReplaceEntityEntry<'guild'>> = [];
		const roleEntries: Array<SnapshotRowReplaceEntityEntry<'guild_role'>> = [];
		const emojiEntries: Array<SnapshotRowReplaceEntityEntry<'guild_emoji'>> = [];
		const stickerEntries: Array<SnapshotRowReplaceEntityEntry<'guild_sticker'>> = [];
		const memberEntries: Array<SnapshotRowReplaceEntityEntry<'guild_member'>> = [];
		const voiceStateEntries: Array<SnapshotRowReplaceEntityEntry<'voice_state'>> = [];
		const presenceGuildIds = new Map<string, Set<string>>();
		for (const guild of availableGuilds) {
			const row = guildRowFromReadyData(guild);
			this.guilds.set(guild.id, row);
			guildEntries.push({key: snapshotGuildKey(guild.id), value: row});
			for (const role of guild.roles) {
				roleEntries.push({key: snapshotGuildRoleKey(guild.id, role.id), value: role});
			}
			for (const member of this.readyGuildMembers(guild)) {
				const key = this.guildMemberKey(guild.id, member.user.id);
				this.guildMembers.set(key, member);
				memberEntries.push({key, value: member});
				this.addPresenceGuildId(presenceGuildIds, member.user.id, guild.id);
				this.userReducer.absorbReadyMember(member.user);
			}
			emojiEntries.push({key: snapshotGuildEmojiKey(guild.id), value: guild.emojis});
			if (guild.stickers) {
				stickerEntries.push({key: snapshotGuildStickerKey(guild.id), value: guild.stickers});
			}
			if (guild.voice_states && guild.voice_states.length > 0) {
				this.voiceStates.set(guild.id, guild.voice_states);
				voiceStateEntries.push({key: snapshotGuildKey(guild.id), value: guild.voice_states});
			}
		}
		this.emitReplaceEntity(emit, 'unavailable_guild', unavailableGuildEntries);
		this.emitReplaceEntity(emit, 'guild', guildEntries);
		this.emitReplaceEntity(emit, 'guild_role', roleEntries);
		this.emitReplaceEntity(emit, 'guild_emoji', emojiEntries);
		this.emitReplaceEntity(emit, 'guild_sticker', stickerEntries);
		this.emitReplaceEntity(emit, 'guild_member', memberEntries);
		this.emitReplaceEntity(emit, 'voice_state', voiceStateEntries);

		const channelEntries = this.channelReducer.initializeReady(data, availableGuilds);
		this.emitReplaceEntity(emit, 'channel', channelEntries.channels);
		this.emitReplaceEntity(emit, 'read_state', channelEntries.readStates);

		this.accountReducer.emitReady(emit);

		const settingsEntries: Array<SnapshotRowReplaceEntityEntry<'user_guild_settings'>> = [];
		for (const settings of data.user_guild_settings ?? []) {
			settingsEntries.push({key: snapshotUserGuildSettingsKey(settings.guild_id), value: settings});
		}
		this.emitReplaceEntity(emit, 'user_guild_settings', settingsEntries);

		const relationshipEntries: Array<SnapshotRowReplaceEntityEntry<'relationship'>> = [];
		for (const relationship of data.relationships ?? []) {
			relationshipEntries.push({key: snapshotRelationshipKey(relationship.id), value: relationship});
		}
		this.emitReplaceEntity(emit, 'relationship', relationshipEntries);

		this.userReducer.emitReady(emit, data, presenceGuildIds);

		return {
			sessionId: data.session_id,
			schemaEpoch: SNAPSHOT_SCHEMA_EPOCH,
			updatedAt: Date.now(),
		};
	}

	applyDispatch(emit: SnapshotEmit, dispatch: SnapshotDispatch): void {
		switch (dispatch.type) {
			case 'GUILD_CREATE':
			case 'GUILD_SYNC':
				this.applyGuildCreate(emit, dispatch.data);
				return;
			case 'GUILD_UPDATE':
				this.applyGuildUpdate(emit, dispatch.data);
				return;
			case 'GUILD_DELETE':
				this.applyGuildDelete(emit, dispatch.data);
				return;
			case 'GUILD_ROLE_CREATE':
			case 'GUILD_ROLE_UPDATE': {
				const payload = dispatch.data;
				this.emitUpsert(emit, 'guild_role', snapshotGuildRoleKey(payload.guild_id, payload.role.id), payload.role);
				return;
			}
			case 'GUILD_ROLE_DELETE': {
				const payload = dispatch.data;
				this.emitDelete(emit, 'guild_role', snapshotGuildRoleKey(payload.guild_id, payload.role_id));
				return;
			}
			case 'GUILD_ROLE_UPDATE_BULK': {
				const payload = dispatch.data;
				for (const role of payload.roles) {
					this.emitUpsert(emit, 'guild_role', snapshotGuildRoleKey(payload.guild_id, role.id), role);
				}
				return;
			}
			case 'GUILD_EMOJIS_UPDATE': {
				const payload = dispatch.data;
				this.emitUpsert(emit, 'guild_emoji', snapshotGuildEmojiKey(payload.guild_id), payload.emojis);
				return;
			}
			case 'GUILD_STICKERS_UPDATE': {
				const payload = dispatch.data;
				this.emitUpsert(emit, 'guild_sticker', snapshotGuildStickerKey(payload.guild_id), payload.stickers);
				return;
			}
			case 'GUILD_COUNTS_UPDATE':
				this.applyGuildCountsUpdate(emit, dispatch.data);
				return;
			case 'GUILD_MEMBER_LIST_UPDATE':
				this.applyGuildMemberListUpdate(emit, dispatch.data);
				return;
			case 'GUILD_MEMBER_ADD':
			case 'GUILD_MEMBER_UPDATE':
				this.applyGuildMemberUpsert(emit, dispatch.data);
				return;
			case 'GUILD_MEMBER_REMOVE':
				this.applyGuildMemberRemove(emit, dispatch.data);
				return;
			case 'CHANNEL_CREATE':
				this.channelReducer.applyChannelUpsert(emit, dispatch.data);
				return;
			case 'CHANNEL_UPDATE':
				this.channelReducer.applyChannelUpdate(emit, dispatch.data);
				return;
			case 'CHANNEL_UPDATE_BULK': {
				const payload = dispatch.data;
				for (const channel of payload.channels) {
					this.channelReducer.applyChannelUpdate(emit, channel);
				}
				return;
			}
			case 'CHANNEL_DELETE':
				this.channelReducer.applyChannelDelete(emit, dispatch.data);
				return;
			case 'CHANNEL_PINS_UPDATE':
				this.channelReducer.applyChannelPinsUpdate(emit, dispatch.data);
				return;
			case 'CHANNEL_PINS_ACK':
				this.channelReducer.applyChannelPinsAck(emit, dispatch.data);
				return;
			case 'CHANNEL_RECIPIENT_ADD':
				this.channelReducer.applyChannelRecipientAdd(emit, dispatch.data);
				return;
			case 'CHANNEL_RECIPIENT_REMOVE':
				this.channelReducer.applyChannelRecipientRemove(emit, dispatch.data);
				return;
			case 'PASSIVE_UPDATES':
				this.channelReducer.applyPassiveUpdates(emit, dispatch.data);
				this.applyPassiveVoiceStates(emit, dispatch.data);
				return;
			case 'VOICE_STATE_UPDATE':
				if (dispatch.data.guild_id) {
					this.applyVoiceStateUpdate(emit, dispatch.data.guild_id, dispatch.data);
				}
				return;
			case 'MESSAGE_CREATE':
				this.channelReducer.applyMessageCreate(emit, dispatch.data);
				return;
			case 'THREAD_CREATE':
			case 'THREAD_UPDATE':
				if (this.threadsActive(dispatch.data.guild_id)) {
					this.channelReducer.applyThreadUpsert(emit, dispatch.data);
				}
				return;
			case 'THREAD_DELETE':
				if (this.threadsActive(dispatch.data.guild_id)) {
					this.channelReducer.applyThreadDelete(emit, dispatch.data);
				}
				return;
			case 'THREAD_LIST_SYNC':
				if (this.threadsActive(dispatch.data.guild_id)) {
					this.channelReducer.applyThreadListSync(emit, dispatch.data);
				}
				return;
			case 'THREAD_MEMBER_UPDATE':
				if (this.threadsActive(dispatch.data.guild_id)) {
					this.channelReducer.applyThreadMemberUpdate(emit, dispatch.data);
				}
				return;
			case 'THREAD_MEMBERS_UPDATE':
				if (this.threadsActive(dispatch.data.guild_id)) {
					this.channelReducer.applyThreadMembersUpdate(emit, dispatch.data);
				}
				return;
			case 'MESSAGE_ACK':
				this.channelReducer.applyMessageAck(emit, dispatch.data);
				return;
			case 'AUTH_SESSION_CHANGE':
				this.accountReducer.applyAuthSessionChange(emit, dispatch.data);
				return;
			case 'USER_SETTINGS_UPDATE':
				this.accountReducer.applyUserSettingsUpdate(emit, dispatch.data);
				return;
			case 'USER_NOTE_UPDATE':
				this.accountReducer.applyUserNoteUpdate(emit, dispatch.data);
				return;
			case 'USER_PINNED_DMS_UPDATE':
				this.accountReducer.applyUserPinnedDmsUpdate(emit, dispatch.data);
				return;
			case 'USER_CONNECTIONS_UPDATE':
				this.accountReducer.applyUserConnectionsUpdate(emit, dispatch.data);
				return;
			case 'WEBAUTHN_CREDENTIALS_UPDATE':
				this.accountReducer.applyWebAuthnCredentialsUpdate(emit, dispatch.data);
				return;
			case 'USER_GUILD_SETTINGS_UPDATE': {
				const payload = dispatch.data;
				this.emitUpsert(emit, 'user_guild_settings', snapshotUserGuildSettingsKey(payload.guild_id), payload);
				return;
			}
			case 'RELATIONSHIP_ADD':
				this.applyRelationship(emit, dispatch.data);
				return;
			case 'RELATIONSHIP_UPDATE':
				this.applyRelationship(emit, dispatch.data);
				return;
			case 'RELATIONSHIP_REMOVE':
				this.emitDelete(emit, 'relationship', snapshotRelationshipKey(dispatch.data.id));
				return;
			case 'USER_UPDATE':
				this.userReducer.applyUserUpdate(emit, dispatch.data);
				return;
			case 'PRESENCE_UPDATE':
				this.applyPresenceUpdate(emit, dispatch.data);
				return;
			case 'PRESENCE_UPDATE_BULK': {
				const payload = dispatch.data;
				for (const presence of payload.presences) {
					this.applyPresenceUpdate(emit, {...presence, guild_id: payload.guild_id ?? presence.guild_id});
				}
				return;
			}
			case 'FAVORITE_MEME_CREATE':
				this.accountReducer.applyFavoriteMemeCreate(emit, dispatch.data);
				return;
			case 'FAVORITE_MEME_UPDATE':
				this.accountReducer.applyFavoriteMemeUpdate(emit, dispatch.data);
				return;
			case 'FAVORITE_MEME_DELETE':
				this.accountReducer.applyFavoriteMemeDelete(emit, dispatch.data);
				return;
			default:
				unsupportedSnapshotDispatch(dispatch);
		}
	}

	private threadsActive(guildId: string | null | undefined): boolean {
		return guildId != null && this.guilds.get(guildId)?.threads_active === true;
	}

	private applyGuildCreate(emit: SnapshotEmit, data: GuildReadyData): void {
		if (isUnavailableGuildCreate(data)) {
			this.applyGuildDelete(emit, {id: data.id, unavailable: true});
			return;
		}
		this.unavailableGuilds.delete(data.id);
		this.emitDelete(emit, 'unavailable_guild', snapshotGuildKey(data.id));
		const row = guildRowFromReadyData(data);
		this.guilds.set(data.id, row);
		this.emitUpsert(emit, 'guild', snapshotGuildKey(data.id), row);
		this.emitDeleteByPrefix(emit, 'guild_role', snapshotGuildScopedPrefix(data.id));
		for (const role of data.roles) {
			this.emitUpsert(emit, 'guild_role', snapshotGuildRoleKey(data.id, role.id), role);
		}
		this.emitUpsert(emit, 'guild_emoji', snapshotGuildEmojiKey(data.id), data.emojis);
		if (data.stickers) {
			this.emitUpsert(emit, 'guild_sticker', snapshotGuildStickerKey(data.id), data.stickers);
		} else {
			this.emitDelete(emit, 'guild_sticker', snapshotGuildStickerKey(data.id));
		}
		const guildScopedPrefix = snapshotGuildScopedPrefix(data.id);
		this.emitDeleteByPrefix(emit, 'guild_member', guildScopedPrefix);
		this.deleteGuildMembers(guildScopedPrefix);
		for (const member of this.readyGuildMembers(data)) {
			this.applyGuildMemberUpsert(emit, {...member, guild_id: data.id});
		}
		this.writeGuildVoiceStates(emit, data.id, data.voice_states ?? []);
		this.channelReducer.applyGuildCreate(emit, data);
	}

	private applyGuildUpdate(emit: SnapshotEmit, data: WireGuild): void {
		const row = guildRowFromWireGuild(data, this.guilds.get(data.id));
		if (row === null) {
			return;
		}
		this.guilds.set(data.id, row);
		this.emitUpsert(emit, 'guild', snapshotGuildKey(data.id), row);
	}

	private applyGuildCountsUpdate(emit: SnapshotEmit, data: SnapshotGuildCountsUpdatePayload): void {
		for (const count of data.counts ?? []) {
			this.applyGuildCountUpdate(emit, count.guild_id, count.member_count, count.online_count);
		}
	}

	private applyGuildMemberListUpdate(emit: SnapshotEmit, data: SnapshotGuildMemberListUpdatePayload): void {
		this.applyGuildCountUpdate(emit, data.guild_id, data.member_count, data.online_count);
	}

	private applyGuildCountUpdate(emit: SnapshotEmit, guildId: string, memberCount: number, onlineCount: number): void {
		const row = this.guilds.get(guildId);
		if (!row) return;
		const next = {
			...row,
			member_count: memberCount,
			online_count: onlineCount,
		};
		this.guilds.set(guildId, next);
		this.emitUpsert(emit, 'guild', snapshotGuildKey(guildId), next);
	}

	private applyGuildDelete(emit: SnapshotEmit, data: SnapshotGuildDeletePayload): void {
		const guildScopedPrefix = snapshotGuildScopedPrefix(data.id);
		this.guilds.delete(data.id);
		this.emitDelete(emit, 'guild', snapshotGuildKey(data.id));
		this.emitDeleteByPrefix(emit, 'guild_role', guildScopedPrefix);
		this.emitDelete(emit, 'guild_emoji', snapshotGuildEmojiKey(data.id));
		this.emitDelete(emit, 'guild_sticker', snapshotGuildStickerKey(data.id));
		this.emitDeleteByPrefix(emit, 'guild_member', guildScopedPrefix);
		if (data.unavailable !== true) {
			this.emitDelete(emit, 'user_guild_settings', snapshotUserGuildSettingsKey(data.id));
		}
		this.deleteGuildMembers(guildScopedPrefix);
		this.writeGuildVoiceStates(emit, data.id, []);
		this.channelReducer.deleteGuild(emit, data.id);
		this.userReducer.applyGuildDelete(emit, data.id);
		if (data.unavailable === true) {
			const row = unavailableGuildRow(data.id);
			this.unavailableGuilds.set(data.id, row);
			this.emitUpsert(emit, 'unavailable_guild', snapshotGuildKey(data.id), row);
		} else {
			this.unavailableGuilds.delete(data.id);
			this.emitDelete(emit, 'unavailable_guild', snapshotGuildKey(data.id));
		}
	}

	private applyGuildMemberUpsert(emit: SnapshotEmit, data: SnapshotGuildMemberPayload): void {
		const key = this.guildMemberKey(data.guild_id, data.user.id);
		const member = this.memberWithoutGuildId(data);
		this.guildMembers.set(key, member);
		this.emitUpsert(emit, 'guild_member', key, member);
		this.userReducer.applyGuildMember(emit, data.user, data.guild_id);
	}

	private applyGuildMemberRemove(emit: SnapshotEmit, data: SnapshotGuildMemberRemovePayload): void {
		this.guildMembers.delete(this.guildMemberKey(data.guild_id, data.user.id));
		this.emitDelete(emit, 'guild_member', this.guildMemberKey(data.guild_id, data.user.id));
		this.userReducer.removeGuildMember(emit, data.user, data.guild_id);
	}

	private applyRelationship(emit: SnapshotEmit, data: RelationshipWire): void {
		this.emitUpsert(emit, 'relationship', snapshotRelationshipKey(data.id), data);
		this.userReducer.applyRelationship(emit, data);
	}

	private applyPresenceUpdate(emit: SnapshotEmit, presence: PresenceRecord): void {
		this.userReducer.applyPresenceUpdate(emit, presence);
	}

	private applyPassiveVoiceStates(emit: SnapshotEmit, data: SnapshotPassiveUpdatesPayload): void {
		for (const voiceState of data.voice_states ?? []) {
			this.applyVoiceStateUpdate(emit, data.guild_id, voiceState);
		}
	}

	private applyVoiceStateUpdate(emit: SnapshotEmit, guildId: string, update: SnapshotVoiceStateUpdatePayload): void {
		const connectionId = update.connection_id;
		if (!connectionId) {
			return;
		}
		const current = this.voiceStates.get(guildId) ?? [];
		const next = current.filter(
			(existing) => existing.connection_id !== connectionId && !isStaleSessionVoiceState(existing, update),
		);
		if (update.channel_id) {
			next.push({...update, guild_id: guildId, channel_id: update.channel_id, connection_id: connectionId});
		} else if (next.length === current.length) {
			return;
		}
		this.writeGuildVoiceStates(emit, guildId, next);
	}

	private writeGuildVoiceStates(emit: SnapshotEmit, guildId: string, voiceStates: ReadonlyArray<VoiceState>): void {
		if (voiceStates.length === 0) {
			this.voiceStates.delete(guildId);
			this.emitDelete(emit, 'voice_state', snapshotGuildKey(guildId));
			return;
		}
		this.voiceStates.set(guildId, voiceStates);
		this.emitUpsert(emit, 'voice_state', snapshotGuildKey(guildId), voiceStates);
	}

	private readyGuildMembers(guild: GuildReadyData): Array<GuildMemberData> {
		const members = new Map<string, GuildMemberData>();
		for (const member of guild.members) {
			members.set(member.user.id, member);
		}
		for (const voiceState of guild.voice_states ?? []) {
			const member = voiceState.member;
			if (member) {
				members.set(member.user.id, member);
			}
		}
		return Array.from(members.values());
	}

	private memberWithoutGuildId(member: SnapshotGuildMemberPayload): GuildMemberData {
		const {guild_id: _guildId, ...data} = member;
		return data;
	}

	private guildMemberKey(guildId: string, userId: string): SnapshotGuildMemberKey {
		return snapshotGuildMemberKey(guildId, userId);
	}

	private addPresenceGuildId(presenceGuildIds: Map<string, Set<string>>, userId: string, guildId: string): void {
		let guildIds = presenceGuildIds.get(userId);
		if (!guildIds) {
			guildIds = new Set<string>();
			presenceGuildIds.set(userId, guildIds);
		}
		guildIds.add(guildId);
	}

	private deleteGuildMembers(guildScopedPrefix: string): void {
		for (const key of this.guildMembers.keys()) {
			if (!key.startsWith(guildScopedPrefix)) {
				continue;
			}
			this.guildMembers.delete(key);
		}
	}

	private emitUpsert<E extends SnapshotEntity>(
		emit: SnapshotEmit,
		entity: E,
		key: SnapshotEntityKeyMap[E],
		value: SnapshotEntityRowMap[E],
	): void {
		emit({kind: 'upsert', entity, key, value} as SnapshotRowOp);
	}

	private emitDelete<E extends SnapshotEntity>(emit: SnapshotEmit, entity: E, key: SnapshotEntityKeyMap[E]): void {
		emit({kind: 'delete', entity, key} as SnapshotRowOp);
	}

	private emitDeleteByPrefix(emit: SnapshotEmit, entity: SnapshotEntity, keyPrefix: SnapshotRowKeyPrefix): void {
		emit({kind: 'deleteByPrefix', entity, keyPrefix});
	}

	private emitReplaceEntity<E extends SnapshotEntity>(
		emit: SnapshotEmit,
		entity: E,
		entries: Array<SnapshotRowReplaceEntityEntry<E>>,
	): void {
		emit({kind: 'replaceEntity', entity, entries} as SnapshotRowOp);
	}
}
