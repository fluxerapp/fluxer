// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelWire} from '@app/features/channel/models/Channel';
import type {ReadyPayload} from '@app/features/gateway/events/GatewayReady';
import type {
	SnapshotChannelDeletePayload,
	SnapshotChannelPinsPayload,
	SnapshotChannelRecipientPayload,
	SnapshotChannelUpdatePayload,
	SnapshotMessageAckPayload,
	SnapshotMessageCreatePayload,
	SnapshotPassiveUpdatesPayload,
} from '@app/features/gateway/snapshot/SnapshotDispatchEvent';
import {
	parseSnapshotEntity,
	type SnapshotEmit,
	type SnapshotReadStateRow,
	type SnapshotRowReplaceEntityEntry,
	type SnapshotUserRow,
	snapshotChannelKey,
	snapshotReadStateKey,
} from '@app/features/gateway/snapshot/SnapshotEntities';
import type {ReducerRowMap} from '@app/features/gateway/snapshot/SnapshotReducerBacking';
import type {StateSnapshotEntries} from '@app/features/gateway/snapshot/SnapshotTypes';
import type {GuildReadyData} from '@app/features/gateway/types/GatewayGuildTypes';
import {
	compareReadStateVersions,
	isNewerMessageId,
	parseTimestamp,
} from '@app/features/read_state/state/read_states/shared';
import {ChannelTypes, TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {THREAD_CHANNEL_TYPES, THREAD_ONLY_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import type {Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import {decodeReadStateProto} from '@fluxer/schema/src/domains/read_state/ReadStateProtoCodec';

export interface SnapshotChannelUserAuthority {
	readonly getSelfUserId: () => string | null;
	readonly retainUser: (emit: SnapshotEmit, user: SnapshotUserRow) => void;
}

export interface SnapshotChannelReadyEntries {
	readonly channels: Array<SnapshotRowReplaceEntityEntry<'channel'>>;
	readonly readStates: Array<SnapshotRowReplaceEntityEntry<'read_state'>>;
}

interface SnapshotReadStateChannelInfo {
	readonly lastMessageId: string | null;
	readonly guildId: string | null;
	readonly type: number;
}

export type SnapshotThreadWire = ChannelWire;

export interface SnapshotThreadListSyncPayload {
	readonly guild_id: string;
	readonly channel_ids?: ReadonlyArray<string>;
	readonly threads: ReadonlyArray<SnapshotThreadWire>;
	readonly members?: ReadonlyArray<ThreadMemberResponse>;
}

export interface SnapshotThreadDeletePayload {
	readonly id: string;
	readonly guild_id: string;
}

export type SnapshotThreadMemberUpdatePayload = ThreadMemberResponse & {readonly guild_id: string};

export interface SnapshotThreadMembersUpdatePayload {
	readonly id: string;
	readonly guild_id: string;
	readonly member_count: number;
	readonly added_members?: ReadonlyArray<ThreadMemberResponse>;
	readonly removed_member_ids?: ReadonlyArray<string>;
}

function isMessageChannelType(type: number): boolean {
	return TEXT_BASED_CHANNEL_TYPES.has(type) || THREAD_CHANNEL_TYPES.has(type);
}

function isThreadRow(channel: WireChannel): channel is SnapshotThreadWire {
	return THREAD_CHANNEL_TYPES.has(channel.type);
}

function newestMessageId(a: string | null | undefined, b: string | null | undefined): string | null {
	if (a == null) return b ?? null;
	if (b == null) return a;
	return isNewerMessageId(b, a) ? b : a;
}

function emptyReadState(): SnapshotReadStateRow {
	return {
		ackMessageId: null,
		ackPinTimestamp: 0,
		mentionCount: 0,
		serverVersion: null,
		readStateKnown: false,
		lastMessageId: null,
		guildId: null,
	};
}

export class SnapshotChannelReducer {
	constructor(
		private readonly channels: ReducerRowMap<string, WireChannel>,
		private readonly readStates: ReducerRowMap<string, SnapshotReadStateRow>,
		private readonly users: SnapshotChannelUserAuthority,
	) {}

	load(entries: StateSnapshotEntries): void {
		this.channels.clear();
		for (const [key, value] of parseSnapshotEntity(entries, 'channel')) {
			this.channels.set(key, value);
		}
		this.readStates.clear();
		for (const [key, value] of parseSnapshotEntity(entries, 'read_state')) {
			this.readStates.set(key, value);
		}
	}

	reset(): void {
		this.channels.clear();
		this.readStates.clear();
	}

	initializeReady(data: ReadyPayload, availableGuilds: ReadonlyArray<GuildReadyData>): SnapshotChannelReadyEntries {
		const channels: Array<WireChannel> = [];
		for (const channel of data.private_channels ?? []) {
			channels.push(channel);
		}
		for (const guild of availableGuilds) {
			for (const channel of guild.channels) {
				channels.push({...channel, guild_id: guild.id});
			}
			for (const thread of guild.threads ?? []) {
				channels.push(threadRow(thread, guild.id, undefined));
			}
		}
		const channelEntries: Array<SnapshotRowReplaceEntityEntry<'channel'>> = [];
		for (const channel of channels) {
			this.channels.set(channel.id, channel);
			channelEntries.push({key: snapshotChannelKey(channel.id), value: channel});
		}
		return {channels: channelEntries, readStates: this.buildReadStateEntries(data, channels)};
	}

	applyGuildCreate(emit: SnapshotEmit, data: GuildReadyData): void {
		const threads = data.threads ?? [];
		this.removeMissingGuildChannels(
			emit,
			data.id,
			new Set([...data.channels.map((channel) => channel.id), ...threads.map((thread) => thread.id)]),
		);
		for (const channel of data.channels) {
			const channelRow = {...channel, guild_id: data.id};
			this.channels.set(channel.id, channelRow);
			emit({kind: 'upsert', entity: 'channel', key: snapshotChannelKey(channel.id), value: channelRow});
			this.syncReadStateFromChannel(emit, channelRow);
		}
		for (const thread of threads) {
			this.writeThread(emit, threadRow(thread, data.id, this.channels.get(thread.id)));
		}
	}

	applyThreadUpsert(emit: SnapshotEmit, wire: SnapshotThreadWire): void {
		if (!wire.guild_id || !wire.parent_id || !THREAD_CHANNEL_TYPES.has(wire.type)) {
			return;
		}
		this.writeThread(emit, threadRow(wire, wire.guild_id, this.channels.get(wire.id)));
		if (wire.newly_created !== true) {
			return;
		}
		const parent = this.channels.get(wire.parent_id);
		if (parent == null || !THREAD_ONLY_CHANNEL_TYPES.has(parent.type)) {
			return;
		}
		if (!isNewerMessageId(wire.id, parent.last_message_id)) {
			return;
		}
		const bumped = {...parent, last_message_id: wire.id};
		this.channels.set(bumped.id, bumped);
		emit({kind: 'upsert', entity: 'channel', key: snapshotChannelKey(bumped.id), value: bumped});
		this.writeReadState(emit, bumped.id, {
			...this.readState(bumped.id),
			lastMessageId: wire.id,
			guildId: bumped.guild_id ?? null,
		});
	}

	applyThreadDelete(emit: SnapshotEmit, data: SnapshotThreadDeletePayload): void {
		const existing = this.channels.get(data.id);
		if (existing != null && isThreadRow(existing)) {
			this.deleteChannelAndReadState(emit, data.id);
		}
	}

	applyThreadListSync(emit: SnapshotEmit, data: SnapshotThreadListSyncPayload): void {
		const parents = data.channel_ids ? new Set(data.channel_ids) : null;
		const incoming = new Set(data.threads.map((thread) => thread.id));
		for (const [channelId, channel] of this.channels) {
			if (channel.guild_id !== data.guild_id || !isThreadRow(channel) || incoming.has(channelId)) {
				continue;
			}
			if (channel.thread_metadata?.archived === true) {
				continue;
			}
			if (parents != null && (channel.parent_id == null || !parents.has(channel.parent_id))) {
				continue;
			}
			this.deleteChannelAndReadState(emit, channelId);
		}
		const members = new Map((data.members ?? []).map((member) => [member.id, member]));
		for (const thread of data.threads) {
			const member = thread.member ?? (thread.id ? members.get(thread.id) : undefined);
			this.writeThread(
				emit,
				threadRow(member ? {...thread, member} : thread, data.guild_id, this.channels.get(thread.id)),
			);
		}
	}

	applyThreadMemberUpdate(emit: SnapshotEmit, data: SnapshotThreadMemberUpdatePayload): void {
		if (!data.id || data.user_id !== this.users.getSelfUserId()) {
			return;
		}
		const existing = this.channels.get(data.id);
		if (existing == null || !isThreadRow(existing)) {
			return;
		}
		const {guild_id: _guildId, ...member} = data;
		this.writeThread(emit, {...existing, member});
	}

	applyThreadMembersUpdate(emit: SnapshotEmit, data: SnapshotThreadMembersUpdatePayload): void {
		const existing = this.channels.get(data.id);
		if (existing == null || !isThreadRow(existing)) {
			return;
		}
		const selfUserId = this.users.getSelfUserId();
		const added = data.added_members?.find((member) => member.user_id === selfUserId);
		const removed = data.removed_member_ids?.some((userId) => userId === selfUserId) ?? false;
		const {member: currentMember, ...rest} = existing;
		const member = removed ? undefined : (added ?? currentMember);
		this.writeThread(emit, {...rest, member_count: data.member_count, ...(member ? {member} : {})});
	}

	deleteGuild(emit: SnapshotEmit, guildId: string): void {
		for (const [channelId, channel] of this.channels) {
			if (channel.guild_id !== guildId) {
				continue;
			}
			this.deleteChannelAndReadState(emit, channelId);
		}
	}

	applyChannelUpsert(emit: SnapshotEmit, channel: WireChannel): void {
		this.channels.set(channel.id, channel);
		emit({kind: 'upsert', entity: 'channel', key: snapshotChannelKey(channel.id), value: channel});
		this.syncReadStateFromChannel(emit, channel);
	}

	applyChannelUpdate(emit: SnapshotEmit, data: SnapshotChannelUpdatePayload): void {
		const existing = this.channels.get(data.id);
		if (existing == null) {
			return;
		}
		const channel: WireChannel = {...existing, ...data};
		this.channels.set(channel.id, channel);
		emit({kind: 'upsert', entity: 'channel', key: snapshotChannelKey(channel.id), value: channel});
		this.syncReadStateFromChannel(emit, channel);
	}

	applyChannelDelete(emit: SnapshotEmit, data: SnapshotChannelDeletePayload): void {
		this.deleteChannelAndReadState(emit, data.id);
		for (const [channelId, channel] of this.channels) {
			if (channel.parent_id === data.id && isThreadRow(channel)) {
				this.deleteChannelAndReadState(emit, channelId);
			}
		}
	}

	applyChannelPinsUpdate(emit: SnapshotEmit, data: SnapshotChannelPinsPayload): void {
		const existing = this.channels.get(data.channel_id);
		if (existing == null) {
			return;
		}
		const channel = {...existing, last_pin_timestamp: data.last_pin_timestamp ?? null};
		this.channels.set(channel.id, channel);
		emit({kind: 'upsert', entity: 'channel', key: snapshotChannelKey(channel.id), value: channel});
	}

	applyChannelPinsAck(emit: SnapshotEmit, data: SnapshotChannelPinsPayload): void {
		const lastPinTimestamp = data.last_pin_timestamp ?? this.channels.get(data.channel_id)?.last_pin_timestamp ?? null;
		if (lastPinTimestamp == null) {
			return;
		}
		this.writeReadState(emit, data.channel_id, {
			...this.readState(data.channel_id),
			ackPinTimestamp: parseTimestamp(lastPinTimestamp),
		});
	}

	applyChannelRecipientAdd(emit: SnapshotEmit, data: SnapshotChannelRecipientPayload): void {
		const existing = this.channels.get(data.channel_id);
		if (existing == null) {
			return;
		}
		this.users.retainUser(emit, data.user);
		const recipients = existing.recipients ?? [];
		if (recipients.some((recipient) => recipient.id === data.user.id)) {
			return;
		}
		const channel = {...existing, recipients: [...recipients, data.user]};
		this.channels.set(channel.id, channel);
		emit({kind: 'upsert', entity: 'channel', key: snapshotChannelKey(channel.id), value: channel});
	}

	applyChannelRecipientRemove(emit: SnapshotEmit, data: SnapshotChannelRecipientPayload): void {
		const existing = this.channels.get(data.channel_id);
		if (existing == null) {
			return;
		}
		if (data.user.id === this.users.getSelfUserId()) {
			this.deleteChannelAndReadState(emit, data.channel_id);
			return;
		}
		const recipients = (existing.recipients ?? []).filter((recipient) => recipient.id !== data.user.id);
		const channel = {...existing, recipients};
		this.channels.set(channel.id, channel);
		emit({kind: 'upsert', entity: 'channel', key: snapshotChannelKey(channel.id), value: channel});
	}

	applyPassiveUpdates(emit: SnapshotEmit, data: SnapshotPassiveUpdatesPayload): void {
		for (const [channelId, lastMessageId] of Object.entries(data.channels)) {
			const existing = this.channels.get(channelId);
			if (
				existing == null ||
				existing.guild_id !== data.guild_id ||
				!isMessageChannelType(existing.type) ||
				!isNewerMessageId(lastMessageId, existing.last_message_id)
			) {
				continue;
			}
			const channel = {...existing, last_message_id: lastMessageId};
			this.channels.set(channel.id, channel);
			emit({kind: 'upsert', entity: 'channel', key: snapshotChannelKey(channel.id), value: channel});
			this.writeReadState(emit, channelId, {
				...this.readState(channelId),
				guildId: data.guild_id,
				lastMessageId,
			});
		}
	}

	applyMessageCreate(emit: SnapshotEmit, data: SnapshotMessageCreatePayload): void {
		const existing = this.channels.get(data.channel_id);
		if (existing == null || !isMessageChannelType(existing.type)) {
			return;
		}
		const previousLastMessageId = existing.last_message_id ?? null;
		if (data.id === existing.last_message_id) {
			if (data.author?.id === this.users.getSelfUserId()) {
				this.syncReadStateFromMessageCreate(emit, data, existing, previousLastMessageId);
			}
			return;
		}
		if (!isNewerMessageId(data.id, existing.last_message_id)) {
			return;
		}
		const channel = {...existing, last_message_id: data.id};
		this.channels.set(channel.id, channel);
		emit({kind: 'upsert', entity: 'channel', key: snapshotChannelKey(channel.id), value: channel});
		this.syncReadStateFromMessageCreate(emit, data, channel, previousLastMessageId);
	}

	applyMessageAck(emit: SnapshotEmit, data: SnapshotMessageAckPayload): void {
		const current = this.readState(data.channel_id);
		if (compareReadStateVersions(data.version, current.serverVersion) < 0) {
			return;
		}
		this.writeReadState(emit, data.channel_id, {
			...current,
			readStateKnown: true,
			ackMessageId: data.message_id === '0' ? null : data.message_id,
			mentionCount: data.mention_count,
			serverVersion: data.version ?? null,
		});
	}

	private buildReadStateEntries(
		data: ReadyPayload,
		channels: ReadonlyArray<WireChannel>,
	): Array<SnapshotRowReplaceEntityEntry<'read_state'>> {
		const channelInfo = new Map<string, SnapshotReadStateChannelInfo>();
		for (const channel of channels) {
			channelInfo.set(channel.id, {
				lastMessageId: channel.last_message_id ?? null,
				guildId: channel.guild_id ?? null,
				type: channel.type,
			});
		}
		const readStates =
			data.read_state_proto == null ? (data.read_states ?? []) : decodeReadStateProto(data.read_state_proto);
		const channelsWithReadState = new Set<string>();
		for (const readState of readStates) {
			channelsWithReadState.add(readState.id);
			const info = channelInfo.get(readState.id);
			this.readStates.set(readState.id, {
				ackMessageId: readState.last_message_id ?? null,
				ackPinTimestamp: parseTimestamp(readState.last_pin_timestamp),
				mentionCount: readState.mention_count ?? 0,
				serverVersion: readState.version ?? null,
				readStateKnown: true,
				lastMessageId: info?.lastMessageId ?? null,
				guildId: info?.guildId ?? null,
			});
		}
		for (const [channelId, info] of channelInfo) {
			if (!isMessageChannelType(info.type) || channelsWithReadState.has(channelId)) {
				continue;
			}
			this.readStates.set(channelId, {
				...emptyReadState(),
				lastMessageId: info.lastMessageId,
				guildId: info.guildId,
			});
		}
		return Array.from(this.readStates.entries(), ([key, value]) => ({key: snapshotReadStateKey(key), value}));
	}

	private removeMissingGuildChannels(emit: SnapshotEmit, guildId: string, nextChannelIds: ReadonlySet<string>): void {
		for (const [channelId, channel] of this.channels) {
			if (channel.guild_id !== guildId || nextChannelIds.has(channelId)) {
				continue;
			}
			this.deleteChannelAndReadState(emit, channelId);
		}
	}

	private deleteChannelAndReadState(emit: SnapshotEmit, channelId: string): void {
		this.channels.delete(channelId);
		emit({kind: 'delete', entity: 'channel', key: snapshotChannelKey(channelId)});
		if (this.readStates.delete(channelId)) {
			emit({kind: 'delete', entity: 'read_state', key: snapshotReadStateKey(channelId)});
		}
	}

	private readState(channelId: string): SnapshotReadStateRow {
		return this.readStates.get(channelId) ?? emptyReadState();
	}

	private writeReadState(emit: SnapshotEmit, channelId: string, row: SnapshotReadStateRow): void {
		this.readStates.set(channelId, row);
		emit({kind: 'upsert', entity: 'read_state', key: snapshotReadStateKey(channelId), value: row});
	}

	private writeThread(emit: SnapshotEmit, thread: SnapshotThreadWire): void {
		this.channels.set(thread.id, thread);
		emit({kind: 'upsert', entity: 'channel', key: snapshotChannelKey(thread.id), value: thread});
		this.syncReadStateFromChannel(emit, thread);
	}

	private syncReadStateFromChannel(emit: SnapshotEmit, channel: WireChannel): void {
		if (!isMessageChannelType(channel.type)) {
			return;
		}
		const current = this.readState(channel.id);
		const privateAck = this.isPrivateAckOnCreateChannel(channel);
		this.writeReadState(emit, channel.id, {
			...current,
			lastMessageId: channel.last_message_id ?? null,
			guildId: channel.guild_id ?? null,
			readStateKnown: privateAck ? true : current.readStateKnown,
			ackMessageId: privateAck ? (channel.last_message_id ?? null) : current.ackMessageId,
		});
	}

	private syncReadStateFromMessageCreate(
		emit: SnapshotEmit,
		message: SnapshotMessageCreatePayload,
		channel: WireChannel,
		previousLastMessageId: string | null,
	): void {
		const current = this.readState(message.channel_id);
		const authoredBySelf = message.author?.id === this.users.getSelfUserId();
		this.writeReadState(emit, message.channel_id, {
			...current,
			lastMessageId: message.id,
			guildId: channel.guild_id ?? message.guild_id ?? current.guildId,
			readStateKnown: authoredBySelf || !current.readStateKnown ? true : current.readStateKnown,
			ackMessageId: authoredBySelf ? message.id : current.readStateKnown ? current.ackMessageId : previousLastMessageId,
			mentionCount: authoredBySelf ? 0 : current.mentionCount,
		});
	}

	private isPrivateAckOnCreateChannel(channel: WireChannel): boolean {
		return (
			channel.last_message_id != null &&
			(channel.type === ChannelTypes.DM ||
				channel.type === ChannelTypes.GROUP_DM ||
				channel.type === ChannelTypes.DM_PERSONAL_NOTES)
		);
	}
}

function threadRow(wire: SnapshotThreadWire, guildId: string, existing: WireChannel | undefined): SnapshotThreadWire {
	const {newly_created: _newlyCreated, member, ...rest} = wire;
	const priorMember = existing != null && isThreadRow(existing) ? existing.member : undefined;
	const resolvedMember = member ?? priorMember;
	return {
		...rest,
		guild_id: guildId,
		last_message_id: newestMessageId(rest.last_message_id, existing?.last_message_id),
		...(resolvedMember ? {member: resolvedMember} : {}),
	};
}
